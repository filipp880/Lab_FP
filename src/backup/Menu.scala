package backup

import scala.io.StdIn
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object Menu:

  final case class Node(title: String,
                        children: List[Node] = Nil,
                        action: Option[String] = None,
                        askArg: Option[String] = None)

  private def leaf(title: String, action: String, askArg: Option[String] = None): Node =
    Node(title, Nil, Some(action), askArg)

  private val tree: Node = Node("Резервное копирование", List(
    Node("Хранилище", List(
      leaf("Создать хранилище", "create"),
      leaf("Открыть хранилище", "open"),
      leaf("Сведения о хранилище", "info")
    )),
    Node("Источники данных", List(
      leaf("Показать список", "sources list"),
      leaf("Добавить источник", "sources add"),
      leaf("Удалить источник", "sources remove", Some("Номер источника: ")),
      leaf("Включить источник", "sources enable", Some("Номер источника: ")),
      leaf("Отключить источник", "sources disable", Some("Номер источника: "))
    )),
    Node("Резервные копии", List(
      leaf("Сканировать изменения", "scan"),
      leaf("Создать копию", "backup"),
      leaf("Список версий", "versions"),
      leaf("Восстановить версию", "restore"),
      leaf("Проверить целостность", "verify")
    )),
    Node("Настройки", List(
      leaf("Перезагрузить конфигурацию", "reload")
    ))
  ))

  private def parse(line: String): Option[Int] =
    if line == "0" then Some(0) else line.toIntOption

  private def draw(node: Node, depth: Int, crumbs: String): String =
    val sb = new StringBuilder
    sb.append("  " * (depth - 1)).append("== ").append(crumbs).append(" ==\n")
    node.children.zipWithIndex.foreach { case (child, i) =>
      val kind = if child.children.nonEmpty then "  [меню]" else ""
      sb.append("  " * depth).append(s"${i + 1}. ${child.title}$kind\n")
    }
    sb.append("  " * depth).append("0. Назад\n")
    sb.toString

  private def runAction(engine: Engine, node: Node): Unit =
    node.action.foreach { base =>
      val cmd = node.askArg match
        case Some(prompt) =>
          print(prompt)
          Option(StdIn.readLine()).map(_.trim).filter(_.nonEmpty) match
            case Some(value) => s"$base $value"
            case None        => base
        case None => base
      println(s"\n--- $cmd ---")
      Try(engine.handle(cmd)) match
        case Failure(e) =>
          println(s"Внутренняя ошибка: ${e.getMessage}")
        case Success(_) => ()
    }

  def run(engine: Engine, log: Logger): Unit =
    val path = ListBuffer(tree)
    var alive = true
    while alive do
      val node = path.last
      val crumbs = path.map(_.title).mkString(" > ")
      println()
      print(draw(node, path.size, crumbs))
      print("Выберите пункт: ")
      val line = Option(StdIn.readLine()) match
        case None =>
          alive = false
          ""
        case Some(l) => l.trim
      if alive then
        parse(line) match
          case Some(0) =>
            if path.size > 1 then path.remove(path.size - 1)
            else
              println("\nМеню закрыто.")
              alive = false
          case Some(n) if n >= 1 && n <= node.children.size =>
            val child = node.children(n - 1)
            if child.children.nonEmpty then path += child
            else runAction(engine, child)
          case _ =>
            println("Нет такого пункта.")
    log.info("Меню закрыто")