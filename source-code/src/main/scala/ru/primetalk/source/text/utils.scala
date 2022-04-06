package ru.primetalk.source.text

extension [T](list: List[T])
  def intersperse(sep: T, accum: List[T] = Nil): List[T] =
    list match
      case Nil => accum.reverse
      case h1::(l@(h2::t)) => l.intersperse(sep, sep::h1::accum)
      case h :: Nil => (h::accum).reverse

extension (list: List[Text])
  def mkText(start: Text, sep: Text, end: Text): Text =
    list.intersperse(sep).foldLeft(start)(_ + _) + end
  def mkText(sep: Text): Text =
    if list.isEmpty then 
      lift("") 
    else
      list.intersperse(sep).reduce(_ + _)
  def wrapIndent(start: Text, end: Text): Text =
    Div(List(start, Indent(list), end))