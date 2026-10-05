package backup

import java.nio.file.{Files, Paths}

final case class Source(
    path: String,
    includeSubdirs: Boolean,
    exclusions: List[String],
    enabled: Boolean = true,
    lastBackup: Option[String] = None
):
  def status: String =
    val p = Paths.get(path)
    if !Files.exists(p) then "недоступен"
    else if !Files.isReadable(p) then "недоступен"
    else "доступен"

  def available: Boolean = status == "доступен"

object Source:
  def fromJson(j: Json): Source =
    Source(
      path = j.get("path").flatMap(_.asString).getOrElse(""),
      includeSubdirs = j.get("includeSubdirs").flatMap(_.asBool).getOrElse(false),
      exclusions = j.get("exclusions").flatMap(_.asArr).map(_.flatMap(_.asString)).getOrElse(Nil),
      enabled = j.get("enabled").flatMap(_.asBool).getOrElse(true),
      lastBackup = j.get("lastBackup").flatMap(_.asString)
    )

  def toJson(s: Source): Json =
    Json.obj(
      "path" -> Json.str(s.path),
      "includeSubdirs" -> Json.bool(s.includeSubdirs),
      "exclusions" -> Json.arr(s.exclusions.map(Json.str)*),
      "enabled" -> Json.bool(s.enabled),
      "lastBackup" -> s.lastBackup.map(Json.str).getOrElse(Json.Null)
    )