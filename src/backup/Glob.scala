package backup

import java.util.regex.Pattern

/** Простые glob-шаблоны: `*`, `**`, `?`. Символьные классы не поддерживаются. */
object Glob:
  def isValidPattern(p: String): Boolean =
    p != null && p.nonEmpty &&
      !p.contains('[') && !p.contains(']') &&
      !p.contains('{') && !p.contains('}') &&
      !p.contains('\n') && !p.contains('\r')

  def matches(pattern: String, name: String): Boolean =
    if !isValidPattern(pattern) then false
    else Pattern.matches(toRegex(pattern), name)

  private def toRegex(p: String): String =
    val sb = new StringBuilder("^")
    var i = 0
    while i < p.length do
      p(i) match
        case '*' if i + 1 < p.length && p(i + 1) == '*' =>
          sb.append("(?:.*/)?")
          if i + 2 > p.length then i += 2
          else
            while i < p.length - 1 && p(i + 1) == '*' do i += 1
            i += 1
        case '*' => sb.append("[^/]*"); i += 1
        case '?' => sb.append("[^/]"); i += 1
        case c if "\\.[]{}()+-^$|".contains(c) =>
          sb.append('\\').append(c); i += 1
        case c => sb.append(c); i += 1
    sb.append('$').toString