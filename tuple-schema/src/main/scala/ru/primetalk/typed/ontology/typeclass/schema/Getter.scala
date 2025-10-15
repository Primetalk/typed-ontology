package ru.primetalk.typed.ontology.typeclass.schema

import scala.annotation.implicitNotFound

@FunctionalInterface
@implicitNotFound(msg = "Cannot obtain ${Column} of type ${ColumnValue} from ${Value}")
trait Getter[Column, ColumnValue, Value]:
  def apply(rtc: Value): ValueWithSchema[Column, ColumnValue]
