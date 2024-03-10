package ru.primetalk.typed.ontology.simple.meta

import scala.quoted.{Expr, Type, Quotes}
import scala.compiletime.constValue

object SimpleTypesMacro:
  def propertyValueType1Impl[B: Type, P <: SimplePropertyId[?, B]: Type](vp: Expr[ValueOf[P]])(using Quotes): Expr[RecordPropertyValueType[P, B]] =
    '{
      new RecordPropertyValueType[P, B]{
        val property: P = ($vp).value
      }
    }
