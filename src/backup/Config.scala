package backup

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

final case class Config(
    logFile: String,
    keyDirectory: String,
    defaultContainerName: String,
    checkOnStorageOpen: Boolean,
    confirmBeforeBackup: Boolean,
    confirmBeforeRestoreOverwrite: Boolean,
    maximumVersionsPerFile: Int,
    hashAlgorithm: String,
    defaultExclusions: List[String]
)

object Config:
  val Default = Config(
    logFile = "backup.log",
    keyDirectory = "keys",
    defaultContainerName = "backup.store",
    checkOnStorageOpen = true,
    confirmBeforeBackup = true,
    confirmBeforeRestoreOverwrite = true,
    maximumVersionsPerFile = 10,
    hashAlgorithm = "SHA-256",
    defaultExclusions = List("*.tmp", "*.log", "cache", "node_modules")
  )

  val ConfigFileName = "backup.json"

  def defaultPath: Path = Paths.get(ConfigFileName).toAbsolutePath

  def load(path: Path): Either[String, Config] =
    if Files.notExists(path) then Right(Default)
    else
      try
        val text = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        Json.parse(text).flatMap(fromJson).flatMap { cfg =>
          validate(cfg) match
            case Left(err) => Left(err)
            case Right(_)  => Right(cfg)
        }
      catch
        case e: Exception => Left(s"Конфигурационный файл имеет неправильный формат: ${e.getMessage}")

  def fromJson(j: Json): Either[String, Config] =
    def reqString(key: String): Either[String, String] =
      j.get(key).flatMap(_.asString).toRight(s"Конфигурация: отсутствует или некорректно поле '$key'")
    def reqBool(key: String): Either[String, Boolean] =
      j.get(key).flatMap(_.asBool).toRight(s"Конфигурация: отсутствует или некорректно поле '$key'")
    def reqInt(key: String): Either[String, Int] =
      j.get(key).flatMap(_.asInt).toRight(s"Конфигурация: отсутствует или некорректно поле '$key'")
    def reqArrOfStrings(key: String): Either[String, List[String]] =
      j.get(key).flatMap(_.asArr).toRight(s"Конфигурация: отсутствует или некорректно поле '$key'")
        .map(_.flatMap(_.asString))

    for
      _             <- j.asObj.toRight("Конфигурация: ожидался JSON-объект")
      logFile       <- reqString("logFile")
      keyDir        <- reqString("keyDirectory")
      containerName <- reqString("defaultContainerName")
      checkOnOpen   <- reqBool("checkOnStorageOpen")
      confirmBackup <- reqBool("confirmBeforeBackup")
      confirmRestore <- reqBool("confirmBeforeRestoreOverwrite")
      maxVersions   <- reqInt("maximumVersionsPerFile")
      hashAlgo      <- reqString("hashAlgorithm")
      exclusions    <- reqArrOfStrings("defaultExclusions")
    yield Config(logFile, keyDir, containerName, checkOnOpen, confirmBackup,
      confirmRestore, maxVersions, hashAlgo, exclusions)

  def validate(cfg: Config): Either[String, Unit] =
    val algoOk =
      try { Crypto.normalize(cfg.hashAlgorithm); Right(()) }
      catch case e: IllegalArgumentException => Left(e.getMessage)

    if cfg.logFile == null || cfg.logFile.trim.isEmpty then
      Left("Путь к журналу не указан")
    else if cfg.keyDirectory == null || cfg.keyDirectory.trim.isEmpty then
      Left("Каталог для ключей не указан")
    else if cfg.maximumVersionsPerFile < 1 then
      Left("Максимальное количество версий меньше единицы")
    else if algoOk.isLeft then
      Left(algoOk.swap.toOption.get)
    else
      cfg.defaultExclusions.find(p => !Glob.isValidPattern(p)) match
        case Some(bad) => Left(s"Правило исключения имеет неправильный формат: '$bad'")
        case None =>
          val dir = Paths.get(cfg.keyDirectory)
          try
            if Files.exists(dir) then
              if Files.isDirectory(dir) then Right(())
              else Left(s"Каталог для ключей '$dir' существует, но не является каталогом")
            else
              Files.createDirectories(dir)
              Right(())
          catch case e: Exception =>
            Left(s"Каталог для ключей '$dir' не существует и не может быть создан: ${e.getMessage}")

  def toJson(cfg: Config): Json =
    Json.obj(
      "logFile" -> Json.str(cfg.logFile),
      "keyDirectory" -> Json.str(cfg.keyDirectory),
      "defaultContainerName" -> Json.str(cfg.defaultContainerName),
      "checkOnStorageOpen" -> Json.bool(cfg.checkOnStorageOpen),
      "confirmBeforeBackup" -> Json.bool(cfg.confirmBeforeBackup),
      "confirmBeforeRestoreOverwrite" -> Json.bool(cfg.confirmBeforeRestoreOverwrite),
      "maximumVersionsPerFile" -> Json.num(cfg.maximumVersionsPerFile),
      "hashAlgorithm" -> Json.str(cfg.hashAlgorithm),
      "defaultExclusions" -> Json.arr(cfg.defaultExclusions.map(Json.str)*)
    )

  def save(path: Path, cfg: Config): Unit =
    Files.write(path, Json.render(toJson(cfg)).getBytes(StandardCharsets.UTF_8))