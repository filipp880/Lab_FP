package backup

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

final class Logger(logFile: String):
  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  private val logPath: Option[Path] =
    if logFile == null || logFile.trim.isEmpty then None
    else Some(Paths.get(logFile).toAbsolutePath)

  private def writeLine(line: String): Unit =
    logPath match
      case None => ()
      case Some(path) =>
        try
          val parent = path.getParent
          if parent != null then Files.createDirectories(parent)
          Files.write(path, (line + "\n").getBytes(StandardCharsets.UTF_8),
            StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
        catch case e: Exception =>
          System.err.println(s"ВНИМАНИЕ: невозможно записать журнал ${path}: ${e.getMessage}")

  def log(level: String, msg: String): Unit =
    val line = s"${LocalDateTime.now.format(fmt)} [$level] $msg"
    writeLine(line)

  def info(msg: String): Unit = log("INFO", msg)
  def warn(msg: String): Unit = log("WARN", msg)
  def error(msg: String): Unit = log("ERROR", msg)

object Logger:
  val Null: Logger = new Logger("")