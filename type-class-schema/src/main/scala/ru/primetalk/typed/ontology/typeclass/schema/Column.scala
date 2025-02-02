package ru.primetalk.typed.ontology.typeclass.schema

/** @tparam C is a column that could be part of RecordSchema. */
trait Column[C]:
  type Name <: String
