package backup

import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, Paths}

object Scanner:

  private def storageRel(srcIdx: Int, relWithin: String): String =
    s"s$srcIdx/${relWithin.replace('\\', '/')}"

  private def sourcePrefix(srcIdx: Int): String = s"s$srcIdx"

  private def storagePrefixOf(rel: String): Option[String] =
    if rel.startsWith("s") then
      val slash = rel.indexOf('/')
      if slash > 0 then Some(rel.substring(0, slash)) else None
    else None

  def sourceIndexOf(rel: String): Int =
    storagePrefixOf(rel).flatMap(_.stripPrefix("s").toIntOption).getOrElse(-1)

  def resolveRel(meta: StorageMeta, rel: String): Option[Path] =
    val idx = sourceIndexOf(rel)
    if idx < 0 || idx >= meta.sources.length then None
    else
      val within = rel.substring(rel.indexOf('/') + 1)
      Some(Paths.get(meta.sources(idx).path).resolve(within.replace('/', java.io.File.separatorChar)))

  /** Обход одного источника. Возвращает (файлы, ошибки чтения). */
  def walkSource(srcIdx: Int, src: Source, algo: String, defaults: List[String],
                 skipAbsolute: Option[Path] = None): (List[WalkedFile], List[String]) =
    val base = Paths.get(src.path)
    val exclusions = (src.exclusions ++ defaults).distinct
    val files = List.newBuilder[WalkedFile]
    val errors = List.newBuilder[String]

    def isSelf(entry: Path): Boolean =
      skipAbsolute.exists(p => entry.toAbsolutePath.normalize == p)

    def exclude(name: String, rel: String): Boolean =
      exclusions.exists(p => Glob.matches(p, name) || Glob.matches(p, rel))

    def visit(dir: Path, prefix: String): Unit =
      val entries: List[Path] =
        try
          val ds = Files.newDirectoryStream(dir)
          try ds.iterator().asScala.toList
          finally ds.close()
        catch case e: Exception =>
          errors += s"$dir: ${e.getMessage}"
          Nil
      entries.sortBy(_.getFileName.toString).foreach { entry =>
        try
          val name = entry.getFileName.toString
          val rel = if prefix.isEmpty then name else s"$prefix/$name"
          if !isSelf(entry) && !exclude(name, rel) then
            if Files.isDirectory(entry) then
              if src.includeSubdirs then visit(entry, rel)
            else
              val size = Files.size(entry)
              val mtime = Files.getLastModifiedTime(entry).toMillis
              val hash = Crypto.hash(algo, entry)
              files += WalkedFile(storageRel(srcIdx, rel), size, mtime, hash)
        catch
          case e: Exception =>
            errors += s"${entry}: ${e.getMessage}"
      }

    if Files.exists(base) && Files.isDirectory(base) then visit(base, "")
    else if Files.exists(base) && Files.isRegularFile(base) then
      if !isSelf(base) then
        val name = base.getFileName.toString
        val size = Files.size(base)
        val mtime = Files.getLastModifiedTime(base).toMillis
        val hash = Crypto.hash(algo, base)
        files += WalkedFile(storageRel(srcIdx, name), size, mtime, hash)
    else errors += s"${src.path}: источник недоступен"

    (files.result(), errors.result())

  /** Полное сканирование источников и сравнение с последней версией. */
  def scan(meta: StorageMeta, algo: String, defaults: List[String]): ScanPlan =
    val lastSnap: Map[String, FileRef] = meta.lastVersion.map(_.snapshot).getOrElse(Map.empty)
    val lastRecords: List[LastRecord] = lastSnap.toList.flatMap {
      case (rel, fr) => fr.hash.map(h => LastRecord(rel, h, fr.size, sourceIndexOf(rel)))
    }

    val (allFiles, unreadable) = meta.sources.zipWithIndex.foldLeft((List.empty[WalkedFile], List.empty[String])) {
      case ((fs, es), (src, idx)) =>
        if !src.enabled then (fs, es)
        else if !src.available then (fs, es)
        else
          val (f, e) = walkSource(idx, src, algo, defaults, Some(meta.containerPath.toAbsolutePath.normalize))
          (fs ++ f, es ++ e)
    }

    val current: Map[String, WalkedFile] = allFiles.map(f => f.rel -> f).toMap
    val unavailable = meta.sources.zipWithIndex.collect {
      case (src, idx) if src.enabled && !src.available => s"${idx + 1}. ${src.path}"
    }

    val newFiles: List[WalkedFile] =
      current.values
        .filter(f => !lastSnap.contains(f.rel) || lastSnap(f.rel).hash.isEmpty)
        .toList

    val changed: List[(LastRecord, WalkedFile)] =
      current.values
        .filter(f => lastSnap.get(f.rel).exists(lf => lf.hash.exists(_ != f.hash)))
        .map(f => (LastRecord(f.rel, lastSnap(f.rel).hash.getOrElse(""), lastSnap(f.rel).size, sourceIndexOf(f.rel)), f))
        .toList
        .sortBy(_._1.rel)

    val deleted: List[LastRecord] =
      lastRecords
        .filter { rec =>
          !current.contains(rec.rel) &&
            rec.srcIndex >= 0 && rec.srcIndex < meta.sources.length &&
            meta.sources(rec.srcIndex).enabled && meta.sources(rec.srcIndex).available
        }
        .sortBy(_.rel)

    val deletedByKey = deleted.groupBy(r => (r.size, r.hash))
    val newByKey = newFiles.groupBy(f => (f.size, f.hash))
    val renamed: List[(LastRecord, WalkedFile)] =
      deletedByKey.keySet.intersect(newByKey.keySet).toList.sorted.flatMap { k =>
        deletedByKey(k).headOption.flatMap(d => newByKey(k).headOption.map(n => (d, n)))
      }

    ScanPlan(ChangeReport(newFiles.sortBy(_.rel), changed, deleted, renamed, unreadable, unavailable, allFiles.size), current)