package ru.primetalk.typed.ontology.typeclass.schema

import scala.annotation.implicitNotFound

@FunctionalInterface
@implicitNotFound(s"Cannot obtain $Column of type $ColumnValue from $Value")
trait Getter[Column, ColumnValue, Value]:
  def apply(rtc: Value): ColumnValue
