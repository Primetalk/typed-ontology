package ru.primetalk.source.text


/** Representation of source code. */
sealed trait Text

case class Inline(parts: List[String]) extends Text

case class Block(lines: List[Text]) extends Text

def concat(a: Text, b: Text): Text = 
  a match
    case i1@Inline(parts1) =>
      b match
        case Inline(parts2) => Inline(parts1 ++ parts2)
        case Block(lines2) => Block(i1 :: lines2)
    case Block(lines1) =>
      b match
        case i@Inline(parts2) => Block(lines1 ++ List(i))
        case Block(lines2) => Block(lines1 ++ lines2)

def concatList(lst: Text*): Text = lst.reduce(concat)

extension (s: String)
  def asText = Inline(List(s))

def lift(s: String) = 
  s.asText

extension (t: Text)
  def +(other: Text) =
    concat(t, other)
  def +(other: String) =
    concat(t, lift(other))

def lines(t: Text, indent: Int = 0, indentStep: Int = 2): List[String] =
  t match
    case Inline(parts) =>  List(("".padTo(indent, ' ') :: parts).mkString)
    case Block(lines1) =>
      lines1.flatMap(t => lines(t, indent + indentStep))

def showText(t: Text, indentStep: Int = 2): String = 
  lines(t, indentStep = indentStep).mkString("\n")

trait Show[T]:
  def show(t: T): Text

def render[T](t: T, indent: Int = 2)(using s: Show[T]): String =
  showText(s.show(t))
