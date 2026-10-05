package backup

import java.io.{BufferedInputStream, BufferedOutputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.zip.{ZipEntry, ZipException, ZipInputStream, ZipOutputStream}

final case class VerifyResult(ok: Int, damaged: List[String], missing: List[String])

object Container:
  private val MetaEntry = "meta/filerecords.json"
  private val BlobPrefix = "blobs/"

  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  def now(): String = LocalDateTime.now.format(fmt)

  private def entryName(hash: String): String = s"$BlobPrefix$hash"

  // ---------- чтение ----------

  def readMeta(containerPath: Path): Either[String, StorageMeta] =
    if !Files.exists(containerPath) then Left(s"Файл хранилища отсутствует: $containerPath")
    else
      try
        readEntryBytes(containerPath, MetaEntry)
          .flatMap(b => Json.parse(new String(b, StandardCharsets.UTF_8)))
          .flatMap(j => StorageMeta.fromJson(j, containerPath))
      catch
        case e: ZipException   => Left(s"Файл хранилища повреждён: ${e.getMessage}")
        case e: Exception      => Left(s"Не удалось открыть файл хранилища: ${e.getMessage}")

  private def readEntryBytes(containerPath: Path, name: String): Either[String, Array[Byte]] =
    try
      val zin = new ZipInputStream(new BufferedInputStream(Files.newInputStream(containerPath)))
      try
        var found: Option[Array[Byte]] = None
        var e = zin.getNextEntry
        while e != null && found.isEmpty do
          if e.getName == name then
            val out = new ByteArrayOutputStream()
            val buf = new Array[Byte](65536)
            var n = zin.read(buf)
            while n > 0 do
              out.write(buf, 0, n)
              n = zin.read(buf)
            found = Some(out.toByteArray)
          zin.closeEntry()
          e = zin.getNextEntry
        found.toRight(s"В хранилище отсутствует запись '$name'")
      finally zin.close()
    catch case e: Exception => Left(s"Не удалось прочитать файл хранилища: ${e.getMessage}")

  // ---------- запись ----------

  /** Создаёт новый zip-контейнер: копирует существующие записи (кроме метаданных),
    * добавляет новые blob-записи и в самом конце пишет метаданные.
    */
  private def writeZip(containerPath: Path, meta: StorageMeta,
                       extraBlobs: List[(String, Array[Byte])],
                       encryptionKey: Option[String]): Unit =
    val tmp = containerPath.resolveSibling(containerPath.getFileName.toString + ".tmp")
    val out = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(tmp)))
    try
      if Files.exists(containerPath) then
        val zin = new ZipInputStream(new BufferedInputStream(Files.newInputStream(containerPath)))
        try
          var e = zin.getNextEntry
          while e != null do
            if !e.getName.startsWith("meta/") then
              out.putNextEntry(new ZipEntry(e.getName))
              val buf = new Array[Byte](65536)
              var n = zin.read(buf)
              while n > 0 do
                out.write(buf, 0, n)
                n = zin.read(buf)
              out.closeEntry()
            zin.closeEntry()
            e = zin.getNextEntry
        finally zin.close()

      extraBlobs.foreach { case (hash, plain) =>
        val bytes = encryptionKey.map(k => StreamCipher.xor(k, plain)).getOrElse(plain)
        out.putNextEntry(new ZipEntry(entryName(hash)))
        out.write(bytes)
        out.closeEntry()
      }

      val metaJson = Json.render(StorageMeta.toJson(meta)).getBytes(StandardCharsets.UTF_8)
      out.putNextEntry(new ZipEntry(MetaEntry))
      out.write(metaJson)
      out.closeEntry()
    finally out.close()

    if Files.exists(containerPath) then Files.delete(containerPath)
    Files.move(tmp, containerPath, StandardCopyOption.REPLACE_EXISTING)

  def create(containerPath: Path, name: String, encrypted: Boolean,
             key: Option[String], sources: List[Source]): Either[String, StorageMeta] =
    if Files.exists(containerPath) then Left(s"Файл хранилища уже существует: $containerPath")
    else
      val meta = StorageMeta(
        name = name,
        containerPath = containerPath,
        encrypted = encrypted,
        keyCheck = if encrypted then Crypto.keyCheck(key.getOrElse(""), name) else "",
        created = now(),
        sources = sources,
        versions = Nil,
        fileVersions = Map.empty
      )
      try
        writeZip(containerPath, meta, Nil, None)
        Right(meta)
      catch case e: Exception => Left(s"Не удалось создать хранилище: ${e.getMessage}")

  def open(containerPath: Path, key: Option[String]): Either[String, StorageMeta] =
    readMeta(containerPath).flatMap { meta =>
      if meta.encrypted then
        key match
          case None => Left("Хранилище зашифровано: требуется ключ доступа")
          case Some(k) =>
            if Crypto.keyCheck(k, meta.name) == meta.keyCheck then Right(meta)
            else if meta.keyCheck.isEmpty then Left("Хранилище повреждено: отсутствует ключ проверки")
            else Left("Введён неправильный ключ")
      else Right(meta)
    }

  /** Перезапись метаданных (например, после изменения списка источников). */
  def save(meta: StorageMeta, key: Option[String]): Either[String, Unit] =
    try
      writeZip(meta.containerPath, meta, Nil, key)
      Right(())
    catch case e: Exception => Left(s"Не удалось сохранить хранилище: ${e.getMessage}")

  // ---------- резервное копирование ----------

  def backup(meta: StorageMeta, key: Option[String], cfg: Config,
             plan: ScanPlan, comment: Option[String]): Either[String, (StorageMeta, BackupResult)] =
    import plan.report
    val algo = cfg.hashAlgorithm
    val lastNum = meta.versions.map(_.number).maxOption.getOrElse(0)
    val newNumber = lastNum + 1

    val baseSnap: Map[String, FileRef] = meta.lastVersion.map(_.snapshot).getOrElse(Map.empty)
    var snap = baseSnap

    val blobs = scala.collection.mutable.LinkedHashMap.empty[String, Array[Byte]]
    var skipped = 0
    var hashesOk = 0

    def addBlob(rel: String): Unit =
      (for
        path <- Scanner.resolveRel(meta, rel)
        if Files.exists(path) && Files.isRegularFile(path)
      yield path) match
        case None => skipped += 1
        case Some(path) =>
          try
            val bytes = Files.readAllBytes(path)
            val h = Crypto.hash(algo, bytes)
            plan.current.get(rel) match
              case Some(wf) if h == wf.hash =>
                blobs.put(h, bytes)
                snap = snap.updated(rel, FileRef(Some(h), wf.size))
                hashesOk += 1
              case _ => skipped += 1 // файл изменился во время копирования
          catch case _: Exception => skipped += 1

    report.newFiles.foreach(f => addBlob(f.rel))
    report.changed.foreach { case (_, f) => addBlob(f.rel) }
    report.deleted.foreach(d => snap = snap.updated(d.rel, FileRef(None, 0L)))

    val fileVersions = meta.fileVersions
    val maxV = cfg.maximumVersionsPerFile
    val updatedFileVersions =
      report.newFiles.map(_.rel).toSet
        .union(report.changed.map(_._2.rel).toSet)
        .union(report.deleted.map(_.rel).toSet)
        .foldLeft(fileVersions) { (fv, rel) =>
          val newList = fv.getOrElse(rel, Nil) :+ newNumber
          fv.updated(rel, if newList.length > maxV then newList.takeRight(maxV) else newList)
        }

    val version = VersionInfo(
      number = newNumber,
      timestamp = now(),
      comment = comment,
      result = "success",
      added = report.newFiles.map(_.rel),
      modified = report.changed.map(_._2.rel),
      deleted = report.deleted.map(_.rel),
      renamed = report.renamed.map { case (d, n) => (d.rel, n.rel) },
      bytes = report.totalNewBytes,
      snapshot = snap
    )

    val updatedSources = meta.sources.map { src =>
      if src.enabled && src.available then src.copy(lastBackup = Some(version.timestamp)) else src
    }
    val newMeta = meta.copy(
      versions = meta.versions :+ version,
      fileVersions = updatedFileVersions,
      sources = updatedSources
    )

    try
      writeZip(meta.containerPath, newMeta, blobs.toList, if newMeta.encrypted then key else None)
      Right((newMeta, BackupResult(version, report.newFiles.size, report.changed.size, report.deleted.size, skipped, report.totalNewBytes)))
    catch case e: Exception => Left(s"Не удалось выполнить резервное копирование: ${e.getMessage}")

  // ---------- восстановление ----------

  enum RestoreDecision:
    case Overwrite, Skip, Abort

  def restore(meta: StorageMeta, key: Option[String], algo: String,
              versionNumber: Int, destDir: Path,
              onConflict: String => RestoreDecision): Either[String, (Int, Int, Int)] =
    meta.versions.find(_.number == versionNumber) match
      case None => Left(s"Версия $versionNumber не найдена")
      case Some(version) =>
        if !Files.isDirectory(destDir) then Left(s"Каталог восстановления недоступен: $destDir")
        else
          var restored = 0
          var skipped = 0
          var aborted = false
          version.snapshot.toList.sortBy(_._1).foreach { case (rel, fr) =>
            if !aborted then
              fr.hash match
                case None => () // удалённый на этой версии файл не восстанавливается
                case Some(hash) =>
                  val target = destDir.resolve(rel)
                  val conflict = Files.exists(target)
                  val decision =
                    if !conflict then RestoreDecision.Overwrite
                    else onConflict(rel)
                  decision match
                    case RestoreDecision.Skip  => skipped += 1
                    case RestoreDecision.Abort => aborted = true
                    case RestoreDecision.Overwrite =>
                      try
                        Files.createDirectories(target.getParent)
                        val out = new BufferedOutputStream(Files.newOutputStream(target))
                        try
                          extractBlob(meta.containerPath, hash, if meta.encrypted then key else None, out)
                        finally out.close()
                        restored += 1
                      catch case _: Exception => skipped += 1
          }
          if aborted then Right((restored, skipped, 0))
          else Right((restored, skipped, version.snapshot.size - restored - skipped))

  private def extractBlob(containerPath: Path, hash: String, keyOpt: Option[String], out: java.io.OutputStream): Unit =
    val zin = new ZipInputStream(new BufferedInputStream(Files.newInputStream(containerPath)))
    try
      var e = zin.getNextEntry
      var found = false
      while e != null && !found do
        if e.getName == entryName(hash) then
          found = true
          val src: java.io.InputStream = keyOpt match
            case Some(k) => new XorInputStream(k, zin)
            case None    => zin
          val buf = new Array[Byte](65536)
          var n = src.read(buf)
          while n > 0 do
            out.write(buf, 0, n)
            n = src.read(buf)
        zin.closeEntry()
        e = zin.getNextEntry
      if !found then throw new ZipException(s"Blob $hash не найден в хранилище")
    finally zin.close()

  // ---------- проверка целостности ----------

  def verify(meta: StorageMeta, key: Option[String], algo: String): Either[String, VerifyResult] =
    if !Files.exists(meta.containerPath) then Left(s"Файл хранилища отсутствует: ${meta.containerPath}")
    else if meta.encrypted && key.isEmpty then Left("Хранилище зашифровано, но ключ не задан")
    else
      val refs = meta.versions.flatMap(_.snapshot.values.flatMap(_.hash).toList).distinct
      val refSet = refs.toSet
      try
        val zin = new ZipInputStream(new BufferedInputStream(Files.newInputStream(meta.containerPath)))
        try
          var ok = 0
          var damaged = List.empty[String]
          var found = Set.empty[String]
          var e = zin.getNextEntry
          while e != null do
            if e.getName.startsWith(BlobPrefix) then
              val hash = e.getName.stripPrefix(BlobPrefix)
              if refSet(hash) then
                found = found + hash
                val digest =
                  if meta.encrypted then Crypto.hash(algo, new XorInputStream(key.get, zin))
                  else Crypto.hash(algo, zin)
                if digest == hash then ok += 1 else damaged = damaged :+ hash
            zin.closeEntry()
            e = zin.getNextEntry
          val missing = refs.filterNot(found).distinct
          Right(VerifyResult(ok, damaged, missing))
        finally zin.close()
      catch case e: Exception => Left(s"Не удалось проверить хранилище: ${e.getMessage}")