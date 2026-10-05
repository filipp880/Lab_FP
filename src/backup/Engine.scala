package backup

import scala.io.StdIn
import scala.jdk.CollectionConverters.*
import java.nio.file.{Files, Path, Paths}

final class Engine(initialCfg: Config, log: Logger):

  private var cfg: Config = initialCfg
  private var storageMeta: Option[StorageMeta] = None
  private var storageKey: Option[String] = None
  private var pending: Option[ScanPlan] = None

  private def readLineOpt(): Option[String] = Option(StdIn.readLine())

  def handle(line: String): Unit =
    if line.isEmpty then return
    val parts = line.split("\\s+").toList
    val cmd = parts.head.toLowerCase
    val args = parts.tail
    cmd match
      case "create"   => cmdCreate()
      case "open"     => cmdOpen()
      case "info"     => cmdInfo()
      case "sources"  => cmdSources(args)
      case "scan"     => cmdScan()
      case "backup"   => cmdBackup(args)
      case "versions" => cmdVersions()
      case "restore"  => cmdRestore(args)
      case "verify"   => cmdVerify()
      case "reload"   => cmdReload()
      case other =>
        println(s"Неизвестная команда: $other")
        log.warn(s"Неизвестная команда: $other")

  // ---------- вспомогательное ----------

  private def requireStorage: Boolean =
    if storageMeta.isEmpty then
      println("Команда недоступна в текущем состоянии: хранилище не открыто")
      false
    else true

  private def askYesNo(default: Boolean): Boolean =
    var result: Option[Boolean] = None
    while result.isEmpty do
      readLineOpt() match
        case None => result = Some(default)
        case Some(s) =>
          s.trim.toLowerCase match
            case "да" | "д" | "yes" | "y" | "1" | "true"  => result = Some(true)
            case "нет" | "н" | "no" | "n" | "0" | "false" => result = Some(false)
            case "" => result = Some(default)
            case _  => print("Введите да или нет: ")
    result.get

  private def humanBytes(n: Long): String =
    if n < 1024 then s"$n Б"
    else if n < 1024L * 1024 then f"${n / 1024.0}%.0f КБ"
    else if n < 1024L * 1024 * 1024 then f"${n / (1024.0 * 1024)}%.0f МБ"
    else f"${n / (1024.0 * 1024 * 1024)}%.1f ГБ"

  private def sanitize(name: String): String =
    name.replaceAll("""[\\/:*?"<>|]+""", "_").trim

  // ---------- команды ----------

  private def chooseDevice(prompt: String): Option[Path] =
    val roots = java.io.File.listRoots().toList.filter(f => f.exists() && f.canWrite && f.canRead)
    if roots.isEmpty then
      println("Нет доступных устройств")
      None
    else
      println("\nДоступные устройства:\n")
      roots.zipWithIndex.foreach { case (r, i) =>
        println(s"${i + 1}. ${r.getAbsolutePath} — свободно ${humanBytes(r.getUsableSpace)}")
      }
      println("0. Указать каталог вручную")
      print(prompt + " ")
      readLineOpt().flatMap { s =>
        s.trim.toIntOption match
          case Some(0) =>
            print("Путь к каталогу: ")
            readLineOpt().map(_.trim).filter(_.nonEmpty).map(Paths.get(_))
          case Some(n) if n >= 1 && n <= roots.length =>
            val r = roots(n - 1)
            if !r.exists() then
              println("Выбранное устройство отключено")
              None
            else Some(r.toPath)
          case _ =>
            println("Некорректный выбор")
            None
      }

  private def chooseContainer(device: Path): Option[Path] =
    if Files.isDirectory(device) then
      val stores =
        try
          val ds = Files.newDirectoryStream(device)
          try ds.iterator().asScala
              .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".store"))
              .toList.sorted
          finally ds.close()
        catch case _: Exception => Nil
      if stores.isEmpty then
        println("На устройстве нет файлов хранилищ (*.store)")
        print("Укажите путь к файлу хранилища: ")
        readLineOpt().map(_.trim).filter(_.nonEmpty).map(Paths.get(_))
      else
        stores.zipWithIndex.foreach { case (p, i) => println(s"${i + 1}. ${p.getFileName}") }
        println("0. Указать файл вручную")
        print("Выберите файл хранилища: ")
        readLineOpt().flatMap { s =>
          s.trim.toIntOption match
            case Some(0) =>
              print("Путь к файлу хранилища: ")
              readLineOpt().map(_.trim).filter(_.nonEmpty).map(Paths.get(_))
            case Some(n) if n >= 1 && n <= stores.length => Some(stores(n - 1))
            case _ =>
              println("Некорректный выбор")
              None
        }
    else
      print("Укажите путь к файлу хранилища: ")
      readLineOpt().map(_.trim).filter(_.nonEmpty).map(Paths.get(_))

  private def findKeyFile(storageName: String): Option[Path] =
    val f = Paths.get(cfg.keyDirectory).resolve(sanitize(storageName) + ".key")
    if Files.isRegularFile(f) then Some(f) else None

  private def collectSources(): List[Source] =
    val buf = List.newBuilder[Source]
    var more = true
    while more do
      print("Путь источника (Enter — закончить): ")
      readLineOpt() match
        case Some(p) if p.trim.isEmpty => more = false
        case Some(p) =>
          val path = p.trim
          print("Включать вложенные каталоги? [да/нет]: ")
          val rec = askYesNo(default = true)
          print("Исключения (через запятую) []: ")
          val excl = readLineOpt()
            .map(_.trim).filter(_.nonEmpty)
            .map(_.split(",").toList.map(_.trim).filter(_.nonEmpty))
            .getOrElse(cfg.defaultExclusions)
          buf += Source(path, rec, excl.distinct, enabled = true)
        case None => more = false
    buf.result()

  private def createKey(storageName: String): Option[String] =
    val key = Crypto.generateKey()
    println("\nСпособ хранения ключа:")
    println("1. Сохранить ключ рядом с программой")
    println("2. Показать ключ и не сохранять его")
    print("Выберите способ: ")
    readLineOpt().map(_.trim).flatMap {
      case "1" =>
        val dir = Paths.get(cfg.keyDirectory)
        val keyFile = dir.resolve(sanitize(storageName) + ".key")
        try
          Files.createDirectories(dir)
          Files.write(keyFile, key.getBytes("UTF-8"))
          println(s"Ключ сохранён в $keyFile")
          println("ВНИМАНИЕ: потеря каталога с программой или файла ключа сделает хранилище недоступным.")
          Some(key)
        catch
          case e: Exception =>
            println(s"Не удалось сохранить ключ: ${e.getMessage}")
            None
      case "2" =>
        println("\nКлюч доступа к хранилищу:\n")
        println(key)
        println("\nПрограмма не будет сохранять этот ключ.")
        println("Без него открыть хранилище будет невозможно.")
        print("Подтвердите, что ключ сохранён [да/нет]: ")
        if askYesNo(default = false) then Some(key) else None
      case _ =>
        println("Некорректный выбор")
        None
    }

  private def cmdCreate(): Unit =
    if storageMeta.isDefined then
      println("Команда недоступна в текущем состоянии: уже открыто хранилище")
      return
    log.info("Создание хранилища")
    chooseDevice("Выберите устройство:") match
      case None => ()
      case Some(device) =>
        print("Название хранилища: ")
        val name = readLineOpt().map(_.trim).getOrElse("")
        if name.isEmpty then
          println("Название не указано")
          return
        print(s"Имя файла [${cfg.defaultContainerName}]: ")
        val fileName = readLineOpt().map(_.trim).filter(_.nonEmpty).getOrElse(cfg.defaultContainerName)
        val containerPath =
          if Files.isDirectory(device) then device.resolve(fileName)
          else Paths.get(device.toString) // устройство-диск
        print("Использовать шифрование? [да/нет]: ")
        val encrypted = askYesNo(default = false)
        val key = if encrypted then createKey(name) else Some("")
        key match
          case None => println("Создание хранилища отменено")
          case Some(k) if encrypted && k.isEmpty => println("Создание хранилища отменено")
          case Some(k) =>
            println("\nНастройка резервируемых данных:")
            val sources = collectSources()
            if sources.isEmpty then
              val userWantsToContinue =
                print("Список источников пуст. Продолжить? [да/нет]: ")
                askYesNo(default = false)
              if !userWantsToContinue then println("Создание хранилища отменено"); return
            val keyOpt = if encrypted then Some(k) else None
            Container.create(containerPath, name, encrypted, keyOpt, sources) match
              case Left(err) =>
                println(err)
                log.error(s"Ошибка при создании хранилища: $err")
              case Right(meta) =>
                storageMeta = Some(meta)
                storageKey = keyOpt
                pending = None
                println(s"Хранилище создано: $containerPath")
                log.info(s"Создано хранилище '$name': $containerPath, шифрование=${if encrypted then "вкл" else "выкл"}")
                cmdInfo()

  private def cmdOpen(): Unit =
    if storageMeta.isDefined then
      println("Команда недоступна в текущем состоянии: уже открыто хранилище")
      return
    log.info("Открытие хранилища")
    chooseDevice("Выберите устройство:") match
      case None => ()
      case Some(device) =>
        chooseContainer(device) match
          case None => ()
          case Some(path) =>
            if !Files.exists(path) then
              println(s"Файл хранилища отсутствует: $path")
              log.error(s"Файл хранилища отсутствует: $path")
              return
            val meta0: Either[String, StorageMeta] = Container.readMeta(path)
            meta0 match
              case Left(err) =>
                println(err)
                log.error(s"Не удалось открыть хранилище: $err")
                pending = None
                return
              case Right(m0) =>
                var keyOpt: Option[String] = None
                if m0.encrypted then
                  findKeyFile(m0.name) match
                    case Some(f) =>
                      keyOpt = Some(new String(Files.readAllBytes(f), "UTF-8").trim)
                      println(s"Ключ загружен из $f")
                    case None =>
                      print("Введите ключ доступа: ")
                      keyOpt = readLineOpt().map(_.trim).filter(_.nonEmpty)
                Container.open(path, keyOpt) match
                  case Left(err) =>
                    println(err)
                    log.error(s"Неудачная попытка открытия зашифрованного хранилища: $err")
                  case Right(meta) =>
                    storageMeta = Some(meta)
                    storageKey = keyOpt
                    pending = None
                    println("Хранилище открыто.")
                    log.info(s"Открыто хранилище '${meta.name}': $path")
                    cmdInfo()
                    if cfg.checkOnStorageOpen then
                      println("\nАвтоматическая проверка изменений:")
                      cmdScan()

  private def cmdInfo(): Unit =
    if !requireStorage then return
    val meta = storageMeta.get
    println(s"Хранилище: ${meta.name}")
    println(s"Файл: ${meta.containerPath}")
    println(s"Шифрование: ${if meta.encrypted then "включено" else "выключено"}")
    println(s"Источников: ${meta.sources.count(_.enabled)}")
    println(s"Последняя резервная копия: ${meta.lastVersion.map(_.timestamp).getOrElse("—")}")
    println(s"Сохранённых версий: ${meta.versions.size}")

  private def cmdSources(args: List[String]): Unit =
    if !requireStorage then return
    val meta = storageMeta.get
    args.headOption.map(_.toLowerCase) match
      case None | Some("list") =>
        println("Источники резервного копирования:\n")
        if meta.sources.isEmpty then println("(список пуст)")
        meta.sources.zipWithIndex.foreach { case (s, i) =>
          println(s"${i + 1}. ${s.path}")
          println(s"   Вложенные каталоги: ${if s.includeSubdirs then "да" else "нет"}")
          println(s"   Исключения: ${if s.exclusions.isEmpty then "—" else s.exclusions.mkString(", ")}")
          println(s"   Состояние: ${if s.enabled then s.status else "отключён"}")
        }
      case Some("add") => sourcesAdd(meta)
      case Some("remove") => sourcesRemove(meta, args.tail)
      case Some("disable") => sourcesToggle(meta, args.tail, enabled = false)
      case Some("enable") => sourcesToggle(meta, args.tail, enabled = true)
      case Some("edit") => sourcesEdit(meta, args.tail)
      case Some(other) =>
        println(s"Неизвестная подкоманда sources: $other")
        println("Доступно: list, add, remove N, enable N, disable N, edit N")

  private def saveSources(meta: StorageMeta, newSources: List[Source]): Unit =
    val updated = meta.copy(sources = newSources)
    Container.save(updated, storageKey) match
      case Left(err) =>
        println(err)
        log.error(err)
      case Right(_) =>
        storageMeta = Some(updated)
        pending = None
        log.info("Список источников изменён")

  private def sourcesAdd(meta: StorageMeta): Unit =
    println("Добавление источника")
    val newSrc = collectSources()
    saveSources(meta, meta.sources ++ newSrc)

  private def sourcesRemove(meta: StorageMeta, args: List[String]): Unit =
    args.headOption.flatMap(_.toIntOption) match
      case None => println("Укажите номер источника: sources remove N")
      case Some(n) if n < 1 || n > meta.sources.length => println("Некорректный номер источника")
      case Some(n) =>
        saveSources(meta, meta.sources.zipWithIndex.filterNot(_._2 == n - 1).map(_._1))
        log.info(s"Удалён источник №$n")

  private def sourcesToggle(meta: StorageMeta, args: List[String], enabled: Boolean): Unit =
    args.headOption.flatMap(_.toIntOption) match
      case None => println(s"Укажите номер источника: sources ${if enabled then "enable" else "disable"} N")
      case Some(n) if n < 1 || n > meta.sources.length => println("Некорректный номер источника")
      case Some(n) =>
        val src = meta.sources(n - 1)
        saveSources(meta, meta.sources.updated(n - 1, src.copy(enabled = enabled)))
        log.info(s"Источник №$n ${if enabled then "включён" else "отключён"}")

  private def sourcesEdit(meta: StorageMeta, args: List[String]): Unit =
    args.headOption.flatMap(_.toIntOption) match
      case None => println("Укажите номер источника: sources edit N")
      case Some(n) if n < 1 || n > meta.sources.length => println("Некорректный номер источника")
      case Some(n) =>
        val src = meta.sources(n - 1)
        println(s"Измените параметры источника №$n (${src.path})")
        print("Изменять путь? [да/нет]: ")
        val changePath = askYesNo(default = false)
        val newPath = if changePath then
          print("Путь: ")
          readLineOpt().map(_.trim).filter(_.nonEmpty).getOrElse(src.path)
        else src.path
        print(s"Вложенные каталоги [${if src.includeSubdirs then "да" else "нет"}]: ")
        val rec = askYesNo(src.includeSubdirs)
        print("Исключения (через запятую) []: ")
        val excl = readLineOpt()
          .map(_.trim).filter(_.nonEmpty)
          .map(_.split(",").toList.map(_.trim).filter(_.nonEmpty))
          .getOrElse(src.exclusions)
        saveSources(meta, meta.sources.updated(n - 1, src.copy(path = newPath, includeSubdirs = rec, exclusions = excl.distinct)))
        log.info(s"Источник №$n изменён")

  private def printChanges(plan: ScanPlan): Unit =
    val r = plan.report
    println("Обнаружены изменения:\n")
    println(s"Новые файлы: ${r.newFiles.size}")
    println(s"Изменённые файлы: ${r.changed.size}")
    println(s"Удалённые файлы: ${r.deleted.size}")
    if r.renamed.nonEmpty then
      println(s"Переименованные/перемещённые: ${r.renamed.size}")
      r.renamed.take(10).foreach { case (d, n) => println(s"   ${d.rel} -> ${n.rel}") }
    println(s"Недоступные файлы: ${r.unreadable.size}")
    r.unreadable.take(10).foreach(u => println(s"   $u"))
    r.unavailableSources.foreach(u => println(s"Недоступный источник: $u"))
    println(s"Общий объём новых данных: ${humanBytes(r.totalNewBytes)}")

  private def cmdScan(): Unit =
    if !requireStorage then return
    log.info("Начало проверки изменений")
    val meta = storageMeta.get
    val plan = Scanner.scan(meta, cfg.hashAlgorithm, cfg.defaultExclusions)
    pending = Some(plan)
    val r = plan.report
    printChanges(plan)
    log.info(s"Проверка завершена: новых ${r.newFiles.size}, изменённых ${r.changed.size}, удалённых ${r.deleted.size}, недоступных ${r.unreadable.size}")

  private def cmdBackup(args: List[String]): Unit =
    if !requireStorage then return
    (pending, storageMeta) match
      case (None, _) =>
        println("Сначала выполните scan")
      case (Some(_), None) =>
        println("Команда недоступна в текущем состоянии: хранилище не открыто")
      case (Some(plan), Some(meta)) =>
        if plan.report.isEmpty then
          println("Изменений не обнаружено.")
          log.info("Резервное копирование не требуется: изменения не обнаружены")
          pending = None
          return
        val comment =
          val c = args.mkString(" ")
          if c.isEmpty then None else Some(c)
        if cfg.confirmBeforeBackup then
          print("Подтвердить добавление изменений? [да/нет]: ")
          if !askYesNo(default = false) then
            println("Добавление изменений отменено")
            return
        // контроль свободного места
        val rootPath = meta.containerPath.getRoot
        val usable =
          if rootPath != null then new java.io.File(rootPath.toString).getUsableSpace
          else Long.MaxValue
        if usable < plan.report.totalNewBytes then
          println("На устройстве недостаточно свободного места.")
          log.error("Недостаточно свободного места на устройстве")
          return
        log.info(s"Начало резервного копирования (${plan.report.totalNewBytes} байт)")
        Container.backup(meta, storageKey, cfg, plan, comment) match
          case Left(err) =>
            println(err)
            log.error(err)
          case Right((newMeta, res)) =>
            storageMeta = Some(newMeta)
            pending = None
            println("\nРезервное копирование завершено.\n")
            println(s"Добавлено файлов: ${res.added}")
            println(s"Обновлено файлов: ${res.updated}")
            println(s"Отмечено удалённых файлов: ${res.deleted}")
            println(s"Пропущено файлов: ${res.skipped}")
            println(s"Записано данных: ${humanBytes(res.bytes)}")
            println(s"Версия хранилища: ${res.version.number}")
            log.info(s"Резервное копирование завершено: +${res.added} ~${res.updated} -${res.deleted}, пропущено ${res.skipped}, версия ${res.version.number}")

  private def cmdVersions(): Unit =
    if !requireStorage then return
    val meta = storageMeta.get
    if meta.versions.isEmpty then println("Сохранённых версий нет")
    else
      println("Версии хранилища:\n")
      meta.versions.sortBy(_.number).reverse.foreach(v => println(v.summary))

  private def cmdRestore(args: List[String]): Unit =
    if !requireStorage then return
    val meta = storageMeta.get
    if meta.versions.isEmpty then
      println("Сохранённых версий нет")
      return
    val verNum: Option[Int] = args.headOption.map(_.trim) match
      case Some("last") | Some("latest") => meta.versions.map(_.number).maxOption
      case Some(s) => s.toIntOption
      case None    => meta.versions.map(_.number).maxOption
    verNum match
      case None =>
        println("Некорректный номер версии: restore <N|last> <каталог>")
      case Some(n) if !meta.versions.exists(_.number == n) =>
        println(s"Версия $n не найдена")
      case Some(n) =>
        val destS = args.drop(1).headOption
        val dest: Option[Path] = destS.map(s => Paths.get(s)).orElse {
          print("Каталог назначения: ")
          readLineOpt().map(_.trim).filter(_.nonEmpty).map(Paths.get(_))
        }
        dest match
          case None => println("Каталог назначения не указан")
          case Some(destDir) =>
            try Files.createDirectories(destDir)
            catch case _: Exception => ()
            if !Files.isDirectory(destDir) || !Files.isWritable(destDir) then
              println(s"Каталог восстановления недоступен: $destDir")
              return
            log.info(s"Восстановление версии $n в $destDir")
            val onConflict: String => Container.RestoreDecision = rel =>
              if cfg.confirmBeforeRestoreOverwrite then
                print(s"Файл '$rel' уже существует. Перезаписать? [да/нет]: ")
                if askYesNo(default = false) then Container.RestoreDecision.Overwrite
                else Container.RestoreDecision.Skip
              else Container.RestoreDecision.Overwrite
            Container.restore(meta, storageKey, cfg.hashAlgorithm, n, destDir, onConflict) match
              case Left(err) =>
                println(err)
                log.error(err)
              case Right((restored, skipped, _)) =>
                println(s"Восстановлено файлов: $restored, пропущено: $skipped")
                log.info(s"Восстановление завершено: $restored успешно, $skipped пропущено")

  private def cmdVerify(): Unit =
    if !requireStorage then return
    val meta = storageMeta.get
    log.info("Проверка целостности хранилища")
    Container.verify(meta, storageKey, cfg.hashAlgorithm) match
      case Left(err) =>
        println(err)
        log.error(err)
      case Right(res) =>
        println(s"Проверка целостности завершена.")
        println(s"Проверено файлов: ${res.ok}")
        if res.damaged.nonEmpty then
          println(s"Повреждённые файлы (${res.damaged.size}): ${res.damaged.mkString(", ")}")
          log.error(s"Обнаружены повреждения: ${res.damaged.size}")
        if res.missing.nonEmpty then
          println(s"Отсутствующие данные (${res.missing.size})")
          log.error(s"Обнаружено отсутствующих данных: ${res.missing.size}")
        val ok = res.damaged.isEmpty && res.missing.isEmpty
        println(if ok then "Целостность хранилища: OK" else "Целостность хранилища: НАРУШЕНА")
        log.info(s"Проверка целостности: ${res.ok} ok, повреждено ${res.damaged.size}, отсутствует ${res.missing.size}")

  private def cmdReload(): Unit =
    val p = Config.defaultPath
    Config.load(p) match
      case Right(newCfg) =>
        cfg = newCfg
        println(s"Конфигурация перезагружена из $p")
        log.info("Конфигурация перезагружена")
      case Left(err) =>
        println(err)
        log.error(s"Конфигурацию не удалось перезагрузить: $err")