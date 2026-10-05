package backup

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.security.{MessageDigest, SecureRandom}

object Crypto:
  def hash(algo: String, bytes: Array[Byte]): String =
    val md = MessageDigest.getInstance(normalize(algo))
    hex(md.digest(bytes))

  def hash(algo: String, is: InputStream): String =
    val md = MessageDigest.getInstance(normalize(algo))
    val buf = new Array[Byte](65536)
    var n = is.read(buf)
    while n > 0 do
      md.update(buf, 0, n)
      n = is.read(buf)
    hex(md.digest())

  def hash(algo: String, path: java.nio.file.Path): String =
    val in = java.nio.file.Files.newInputStream(path)
    try hash(algo, in)
    finally in.close()

  def normalize(algo: String): String = algo.toUpperCase match
    case "SHA-256" => "SHA-256"
    case "MD5"     => "MD5"
    case other     => throw new IllegalArgumentException(s"Неподдерживаемый способ проверки целостности: $other")

  def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  def generateKey(): String =
    val alphabet = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789".toCharArray
    val rnd = new SecureRandom()
    (0 until 20).map(_ => alphabet(rnd.nextInt(alphabet.length))).mkString.grouped(4).mkString("-")

  def keyCheck(key: String, storageName: String): String =
    hash("SHA-256", (key + ":" + storageName).getBytes(StandardCharsets.UTF_8))

/** Простой потоковый шифр (XOR с псевдослучайным ключевым потоком).
  * Не криптостойкий по-настоящему, но по заданию разрабатывать криптографические
  * алгоритмы не требуется — контейнер просто не читается без ключа.
  */
object StreamCipher:
  def xor(key: String, data: Array[Byte]): Array[Byte] =
    val in = new java.io.ByteArrayInputStream(data)
    val out = new java.io.ByteArrayOutputStream(data.length)
    xorStream(key, in, out)
    out.toByteArray

  /** XOR-трансформация потоков: применяется и при шифровании, и при расшифровке. */
  def xorStream(key: String, in: java.io.InputStream, out: java.io.OutputStream): Unit =
    val md = MessageDigest.getInstance("SHA-256")
    val base = key.getBytes(StandardCharsets.UTF_8)
    val buf = new Array[Byte](65536)
    var blockIndex = 0L
    var n = in.read(buf)
    while n > 0 do
      var off = 0
      while off < n do
        val ctr = java.nio.ByteBuffer.allocate(8).putLong(blockIndex).array()
        val block = md.digest(base ++ ctr)
        var k = 0
        while k < block.length && off < n do
          buf(off) = (buf(off) ^ block(k)).toByte
          k += 1
          off += 1
        blockIndex += 1
      out.write(buf, 0, n)
      n = in.read(buf)

/** Поток, расшифровывающий хранимые байты на лету (XOR той же схемой). */
final class XorInputStream(key: String, underlying: java.io.InputStream) extends java.io.InputStream:
  private val md = MessageDigest.getInstance("SHA-256")
  private val base = key.getBytes(StandardCharsets.UTF_8)
  private var blockIndex = 0L
  private var block: Array[Byte] = Array.emptyByteArray
  private var blockPos = 0

  override def read(): Int =
    val b = new Array[Byte](1)
    val n = read(b, 0, 1)
    if n < 0 then -1 else b(0) & 0xff

  override def read(b: Array[Byte], off: Int, len: Int): Int =
    var got = 0
    var continue = true
    while continue && got < len do
      if blockPos >= block.length then
        val ctr = java.nio.ByteBuffer.allocate(8).putLong(blockIndex).array()
        block = md.digest(base ++ ctr)
        blockPos = 0
        blockIndex += 1
      val need = math.min(len - got, block.length - blockPos)
      val nb = underlying.read(b, off + got, need)
      if nb < 0 then continue = false
      else
        var k = 0
        while k < nb do
          b(off + got + k) = (b(off + got + k) ^ block(blockPos + k)).toByte
          k += 1
        blockPos += nb
        got += nb
    if got == 0 then -1 else got