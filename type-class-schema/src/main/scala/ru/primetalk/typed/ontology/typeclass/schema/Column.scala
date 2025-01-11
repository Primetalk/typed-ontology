package ru.primetalk.typed.ontology.typeclass.schema

/** @tparam T is a column that could be part of RecordSchema */
trait Column[T]:
  type Name <: String
  def name(column: T): Name

object Column:
  inline def infer[T: Column: ValueOf]: T =
    valueOf[T]
