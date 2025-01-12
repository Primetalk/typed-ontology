package ru.primetalk.typed.ontology.typeclass.schema

@FunctionalInterface
trait Getter[Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple]:
  def apply(rtc: RecordTupleValue[Schema, SchemaValue]): ColumnValue
