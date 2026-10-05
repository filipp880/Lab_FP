package backup

enum Json:
  case JObj(fields: List[(String, Json)])
  case JArr(items: List[Json])
  case JStr(value: String)
  case JNum(value: Long)
  case JDouble(value: Double)
  case JBool(value: Boolean)
  case JNull

  def get(key: String): Option[Json] = this match
    case JObj(fs) => fs.collectFirst { case (k, v) if k == key => v }
    case _        => None

  def asString: Option[String] = this match
    case JStr(v) => Some(v)
    case _       => None

  def asLong: Option[Long] = this match
    case JNum(v)              => Some(v)
    case JDouble(v) if v == Math.floor(v) => Some(v.toLong)
    case _                    => None

  def asInt: Option[Int] = asLong.map(_.toInt)

  def asBool: Option[Boolean] = this match
    case JBool(b) => Some(b)
    case _        => None

  def asArr: Option[List[Json]] = this match
    case JArr(xs) => Some(xs)
    case _        => None

  def asObj: Option[List[(String, Json)]] = this match
    case JObj(xs) => Some(xs)
    case _        => None

final class JsonParseException(msg: String) extends Exception(msg)

object Json:
  def obj(fields: (String, Json)*): Json = JObj(fields.toList)
  def arr(items: Json*): Json = JArr(items.toList)
  def str(s: String): Json = JStr(s)
  def num(n: Long): Json = JNum(n)
  def bool(b: Boolean): Json = JBool(b)
  val Null: Json = JNull

  def render(j: Json): String = j match
    case JNull      => "null"
    case JBool(b)   => if b then "true" else "false"
    case JNum(n)    => n.toString
    case JDouble(d) => d.toString
    case JStr(s)    => "\"" + escape(s) + "\""
    case JArr(xs)   => xs.map(render).mkString("[", ",", "]")
    case JObj(fs)   => fs.map((k, v) => "\"" + escape(k) + "\":" + render(v)).mkString("{", ",", "}")

  private def escape(s: String): String =
    s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c    => c.toString
    }

  def parse(s: String): Either[String, Json] =
    try Right(new Parser(s).parse)
    catch case e: JsonParseException => Left(e.getMessage)

  private final class Parser(s: String):
    private var i = 0

    private def fail(msg: String): Nothing = throw new JsonParseException(s"$msg (позиция $i)")

    private def skipWs(): Unit = while i < s.length && s(i).isWhitespace do i += 1

    def parse: Json =
      skipWs()
      val v = value()
      skipWs()
      if i < s.length then fail("Неожиданные символы в конце")
      v

    private def value(): Json =
      skipWs()
      if i >= s.length then fail("Ожидалось значение")
      s(i) match
        case '{' => obj()
        case '[' => array()
        case '"' => JStr(string())
        case 't' | 'f' => bool()
        case 'n' => nullValue()
        case c if c == '-' || c.isDigit => number()
        case c => fail(s"Неожиданный символ '$c'")

    private def obj(): Json =
      i += 1 // {
      val buf = List.newBuilder[(String, Json)]
      skipWs()
      if i < s.length && s(i) == '}' then { i += 1; return JObj(Nil) }
      var done = false
      while !done do
        skipWs()
        val k = string()
        skipWs()
        if i >= s.length || s(i) != ':' then fail("Ожидалось ':'")
        i += 1
        val v = value()
        buf += ((k, v))
        skipWs()
        if i >= s.length then fail("Ожидалось '}'")
        s(i) match
          case ',' => i += 1
          case '}' => i += 1; done = true
          case c   => fail(s"Ожидалось ',' или '}', найдено '$c'")
      JObj(buf.result())

    private def array(): Json =
      i += 1 // [
      val buf = List.newBuilder[Json]
      skipWs()
      if i < s.length && s(i) == ']' then { i += 1; return JArr(Nil) }
      var done = false
      while !done do
        val v = value()
        buf += v
        skipWs()
        if i >= s.length then fail("Ожидалось ']'")
        s(i) match
          case ',' => i += 1
          case ']' => i += 1; done = true
          case c   => fail(s"Ожидалось ',' или ']', найдено '$c'")
      JArr(buf.result())

    private def string(): String =
      if i >= s.length || s(i) != '"' then fail("Ожидалась строка")
      i += 1
      val sb = new StringBuilder
      var done = false
      while !done do
        if i >= s.length then fail("Незакрытая строка")
        val c = s(i)
        c match
          case '"' => i += 1; done = true
          case '\\' =>
            i += 1
            if i >= s.length then fail("Незавершённая escape-последовательность")
            s(i) match
              case '"'  => sb.append('"'); i += 1
              case '\\' => sb.append('\\'); i += 1
              case '/'  => sb.append('/'); i += 1
              case 'b'  => sb.append('\b'); i += 1
              case 'f'  => sb.append('\f'); i += 1
              case 'n'  => sb.append('\n'); i += 1
              case 'r'  => sb.append('\r'); i += 1
              case 't'  => sb.append('\t'); i += 1
              case 'u'  =>
                if i + 4 >= s.length then fail("Некорректное \\uXXXX")
                val hex = s.substring(i + 1, i + 5)
                if !hex.forall(c2 => c2.isDigit || "abcdefABCDEF".contains(c2)) then fail("Некорректное \\uXXXX")
                sb.append(Integer.parseInt(hex, 16).toChar)
                i += 5
              case other => fail(s"Неизвестная escape-последовательность '\\$other'")
          case _ => sb.append(c); i += 1
      sb.result()

    private def bool(): Json =
      if s.startsWith("true", i) then { i += 4; JBool(true) }
      else if s.startsWith("false", i) then { i += 5; JBool(false) }
      else fail("Некорректное логическое значение")

    private def nullValue(): Json =
      if s.startsWith("null", i) then { i += 4; JNull }
      else fail("Некорректное 'null'")

    private def number(): Json =
      val start = i
      if i < s.length && s(i) == '-' then i += 1
      if i >= s.length || !s(i).isDigit then fail("Некорректное число")
      while i < s.length && s(i).isDigit do i += 1
      var isDouble = false
      if i < s.length && s(i) == '.' then
        isDouble = true
        i += 1
        if i >= s.length || !s(i).isDigit then fail("Некорректное число")
        while i < s.length && s(i).isDigit do i += 1
      if i < s.length && (s(i) == 'e' || s(i) == 'E') then
        isDouble = true
        i += 1
        if i < s.length && (s(i) == '+' || s(i) == '-') then i += 1
        if i >= s.length || !s(i).isDigit then fail("Некорректное число")
        while i < s.length && s(i).isDigit do i += 1
      val text = s.substring(start, i)
      if isDouble then JDouble(text.toDouble)
      else
        try JNum(text.toLong)
        catch case _: NumberFormatException => JDouble(text.toDouble)