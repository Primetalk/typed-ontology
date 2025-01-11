package ru.primetalk.typed.ontology.example4

import scala.quoted.*

object PrintMacro {
  def print[T: Type](arg: Expr[T])(using Quotes): Expr[String] =
    import quotes.reflect.report

    arg match
      case '{ type h ; type tail <: Tuple ; scala.runtime.Tuples.cons($x, $y).asInstanceOf[*:[h, tail]]} =>
//      case '{ type tail <: NonEmptyTuple ; $x *: ($y: tail) } =>
        report.errorAndAbort("booo")
      case _ =>
        report.errorAndAbort(s"dooo ${arg.show}")

}
