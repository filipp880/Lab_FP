package backup

import java.nio.file.{Files, Paths}

@main
def backupApp(): Unit =
  val configPath = Config.defaultPath
  if Files.notExists(configPath) then
    try Config.save(configPath, Config.Default)
    catch case _: Exception => ()
  Config.load(configPath) match
    case Left(err) =>
      println(s"Ошибка конфигурации: $err")
    case Right(cfg) =>
      val log = new Logger(cfg.logFile)
      val engine = new Engine(cfg, log)
      Menu.run(engine, log)