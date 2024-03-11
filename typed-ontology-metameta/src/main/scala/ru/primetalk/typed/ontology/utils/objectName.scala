package ru.primetalk.typed.ontology.utils

import scala.quoted.{Expr, Type, Quotes, quotes}

/** Определяет имя переданного объекта. */
inline def nameOf(o: Any): String = ${ObjectNameImpl.nameOfImpl('o)}

/**
  * Вычисляет имя объекта на основе имени класса.
  */
def objectName(o: Any): String =
  val simpleName = o.getClass.getSimpleName
  val l          = simpleName.length
  if simpleName(l - 1) == '$' then simpleName.substring(0, l - 1)
  else simpleName

object ObjectNameImpl:
  def nameOfImpl(o: Expr[Any])(using Quotes): Expr[String] =
    import quotes.reflect.*
    o.asTerm match
      case Inlined(_, _, Ident(name)) => 
        Expr(name)
      case t =>
        report.errorAndAbort(s"Couldn't determine name of $t")
