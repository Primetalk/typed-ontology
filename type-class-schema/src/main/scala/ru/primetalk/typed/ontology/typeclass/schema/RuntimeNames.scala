package ru.primetalk.typed.ontology.typeclass.schema

import scala.compiletime.{constValue, constValueTuple}
import scala.Predef.valueOf

/**
  * Trait provides basic runtime information about type T - the list of field names.
  * Typically this is defined for record schemas.
  */
trait RuntimeNames[T]:
  def names: List[String]

object RuntimeNames:

  inline def names[T <: Tuple]: T = constValueTuple[T]
  final class RuntimeNamesImpl[S](val names: List[String]) extends RuntimeNames[S]
  inline given nonEmpty[S <: Tuple, N <: Tuple](using columnNames: ColumnsNames[S,N]): RuntimeNames[S] = {
    val n = constValueTuple[N]
    
    val names = n.toIArray.map(_.asInstanceOf[String]).toList

    RuntimeNamesImpl[S](names)
  }
