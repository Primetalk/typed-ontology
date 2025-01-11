package ru.primetalk.typed.ontology.typeclass.schema

/** Represents value type of schema or column. Whether it's Schema or Column is defined by usage. */
trait SchemaValueType[SchemaOrColumn]:
  type Value
