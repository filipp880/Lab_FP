package backup

import java.nio.file.Path

/** Запись о файле на конкретной версии хранилища. hash = None означает,
  * что на этой версии файл был удалён.
  */
final case class FileRef(hash: Option[String], size: Long)

final case class VersionInfo(
    number: Int,
    timestamp: String,
    comment: Option[String],
    result: String,
    added: List[String],
    modified: List[String],
    deleted: List[String],
    renamed: List[(String, String)],
    bytes: Long,
    snapshot: Map[String, FileRef]
):
  def summary: String =
    s"$number  $timestamp  +${added.size}  ~${modified.size}  -${deleted.size}  ${comment.getOrElse("")}"

object VersionInfo:
  def fromJson(j: Json): VersionInfo =
    def strs(key: String): List[String] =
      j.get(key).flatMap(_.asArr).map(_.flatMap(_.asString)).getOrElse(Nil)
    def renames: List[(String, String)] =
      j.get("renamed").flatMap(_.asArr).map(arr =>
        arr.flatMap { r =>
          for
            from <- r.get("from").flatMap(_.asString)
            to   <- r.get("to").flatMap(_.asString)
          yield (from, to)
        }
      ).getOrElse(Nil)
    val snap = j.get("snapshot").flatMap(_.asObj).getOrElse(Nil).flatMap {
      case (k, v) =>
        v.get("hash").flatMap(_.asString) match
          case Some(h) => Some(k -> FileRef(Some(h), v.get("size").flatMap(_.asLong).getOrElse(0L)))
          case None    => Some(k -> FileRef(None, 0L))
    }.toMap
    VersionInfo(
      number = j.get("number").flatMap(_.asInt).getOrElse(0),
      timestamp = j.get("timestamp").flatMap(_.asString).getOrElse(""),
      comment = j.get("comment").flatMap(_.asString),
      result = j.get("result").flatMap(_.asString).getOrElse(""),
      added = strs("added"),
      modified = strs("modified"),
      deleted = strs("deleted"),
      renamed = renames,
      bytes = j.get("bytes").flatMap(_.asLong).getOrElse(0L),
      snapshot = snap
    )

  def toJson(v: VersionInfo): Json =
    Json.obj(
      "number" -> Json.num(v.number),
      "timestamp" -> Json.str(v.timestamp),
      "comment" -> v.comment.map(Json.str).getOrElse(Json.Null),
      "result" -> Json.str(v.result),
      "added" -> Json.arr(v.added.map(Json.str)*),
      "modified" -> Json.arr(v.modified.map(Json.str)*),
      "deleted" -> Json.arr(v.deleted.map(Json.str)*),
      "renamed" -> Json.arr(v.renamed.map { case (f, t) =>
        Json.obj("from" -> Json.str(f), "to" -> Json.str(t))
      }*),
      "bytes" -> Json.num(v.bytes),
      "snapshot" -> Json.obj(v.snapshot.toList.sortBy(_._1).map { case (k, fr) =>
        k -> Json.obj(
          "hash" -> fr.hash.map(Json.str).getOrElse(Json.Null),
          "size" -> Json.num(fr.size)
        )
      }*)
    )

final case class StorageMeta(
    name: String,
    containerPath: Path,
    encrypted: Boolean,
    keyCheck: String,
    created: String,
    sources: List[Source],
    versions: List[VersionInfo],
    fileVersions: Map[String, List[Int]]
):
  def lastVersion: Option[VersionInfo] = versions.sortBy(_.number).lastOption

object StorageMeta:
  def toJson(m: StorageMeta): Json =
    val (key, value) = "keyCheck" -> Json.str(m.keyCheck)
    Json.obj(
      "format" -> Json.num(1),
      "name" -> Json.str(m.name),
      "encrypted" -> Json.bool(m.encrypted),
      key -> value,
      "created" -> Json.str(m.created),
      "sources" -> Json.arr(m.sources.map(Source.toJson)*),
      "versions" -> Json.arr(m.versions.map(VersionInfo.toJson)*),
      "fileVersions" -> Json.obj(m.fileVersions.toList.sortBy(_._1).map { case (k, vs) =>
        k -> Json.arr(vs.map(x => Json.num(x.toLong))*)
      }*)
    )

  def fromJson(j: Json, containerPath: Path): Either[String, StorageMeta] =
    for
      _ <- j.asObj.toRight("Хранилище повреждено: ожидался JSON-объект")
      _ <- if j.get("format").flatMap(_.asInt).contains(1) then Right(())
           else Left("Хранилище повреждено: неверный формат")
      name <- j.get("name").flatMap(_.asString).toRight("Хранилище повреждено: отсутствует имя")
      encrypted <- j.get("encrypted").flatMap(_.asBool).toRight("Хранилище повреждено: отсутствует признак шифрования")
      keyCheck <- j.get("keyCheck").flatMap(_.asString).toRight("Хранилище повреждено: отсутствует ключ проверки")
      created <- j.get("created").flatMap(_.asString).toRight("Хранилище повреждено: отсутствует дата создания")
      sources <- j.get("sources").flatMap(_.asArr).toRight("Хранилище повреждено: отсутствует список источников")
      versions <- j.get("versions").flatMap(_.asArr).toRight("Хранилище повреждено: отсутствует список версий")
      fileVersions <- Right(j.get("fileVersions").flatMap(_.asObj).getOrElse(Nil).flatMap {
        case (k, v) => v.asArr.map(_.flatMap(_.asInt)).filter(_.nonEmpty).map(k -> _).toList
      }.toMap)
    yield StorageMeta(name, containerPath, encrypted, keyCheck, created,
      sources.map(Source.fromJson), versions.map(VersionInfo.fromJson), fileVersions)

case class WalkedFile(rel: String, size: Long, mtime: Long, hash: String)

/** Запись о файле из последней сохранённой версии. */
final case class LastRecord(rel: String, hash: String, size: Long, srcIndex: Int)

final case class ChangeReport(
    newFiles: List[WalkedFile],
    changed: List[(LastRecord, WalkedFile)],
    deleted: List[LastRecord],
    renamed: List[(LastRecord, WalkedFile)],
    unreadable: List[String],
    unavailableSources: List[String],
    scannedCount: Int
):
  def totalNewBytes: Long = newFiles.map(_.size).sum + changed.map(_._2.size).sum
  def isEmpty: Boolean = newFiles.isEmpty && changed.isEmpty && deleted.isEmpty && renamed.isEmpty && unreadable.isEmpty && unavailableSources.isEmpty

/** Отсканированное состояние + найденные изменения (до подтверждения backup). */
final case class ScanPlan(report: ChangeReport, current: Map[String, WalkedFile])

final case class BackupResult(
    version: VersionInfo,
    added: Int,
    updated: Int,
    deleted: Int,
    skipped: Int,
    bytes: Long
)