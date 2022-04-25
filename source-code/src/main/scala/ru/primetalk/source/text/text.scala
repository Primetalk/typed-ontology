package ru.primetalk.source.text


/** Representation of source code. */
sealed trait Text

case class Span(parts: List[String]) extends Text

case class Div(lines: List[Text]) extends Text

case class Indent(lines: List[Text]) extends Text

def concat(a: Text, b: Text): Text = 
  a match
    case i1@Span(parts1) =>
      b match
        case Span(parts2) => Span(parts1 ++ parts2)
        case Div(lines2) =>
          Div( 
            lines2 match
              case Span(s) :: tail =>
                Span(parts1 ::: s) :: tail
              case _ =>
                i1 :: lines2
          )
        case Indent(_) => Div(List(i1, b))
    case Div(lines1) =>
      Div(
        b match
          case i@Span(parts2) => 
            if lines1.isEmpty then 
              List(b)
            else
              lines1.init ++ List(lines1.last + b)
          case Div(lines2) => lines1 ++ lines2
          case Indent(_) => 
            if lines1.isEmpty then 
              List(b)
            else
              lines1.init ++ List(lines1.last + b)
      )
    case Indent(lines1) =>
      Div(
        b match
          case Span(_) => List(a, b)
          case Div(lst) => a :: lst
          case Indent(lines2) => lines1 ++ lines2
      )

def concatList(lst: Text*): Text = lst.reduce(concat)

extension (s: String)
  def asText = Span(List(s))

def lift(s: String) = 
  s.asText

extension (t: Text)
  def +(other: Text) =
    concat(t, other)
  def +(other: String) =
    concat(t, lift(other))

case class IndentationStyle(indentStep: Int = 2, indentChar: Char = ' ')

val JavaIndentation = IndentationStyle(4, ' ')
val ScalaIndentation = IndentationStyle(2, ' ')
val GolangIndentation = IndentationStyle(1, '\t')

def lines(t: Text, indent: Int = 0, indentStyle: IndentationStyle = ScalaIndentation): List[String] =
  t match
    case Span(parts) => 
      List(("".padTo(indent, indentStyle.indentChar) :: parts).mkString)
    case Div(lines1) =>
      lines1.flatMap(t => lines(t, indent, indentStyle))
    case Indent(lines1) =>
      lines1.flatMap(t => lines(t, indent + indentStyle.indentStep, indentStyle))

def showText(t: Text, indentStyle: IndentationStyle = ScalaIndentation): String = 
  lines(t, indentStyle = indentStyle).mkString("\n")

trait Show[T]:
  def show(t: T): Text

def render[T](t: T, indentStyle: IndentationStyle = ScalaIndentation)(using s: Show[T]): String =
  showText(s.show(t), indentStyle = indentStyle)

def show[T](t: T)(using s: Show[T]): Text =
  s.show(t)
