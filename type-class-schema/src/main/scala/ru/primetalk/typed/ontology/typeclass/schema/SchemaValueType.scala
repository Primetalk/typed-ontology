package ru.primetalk.typed.ontology.typeclass.schema

/** Represents value type of schema or column. Whether it's Schema or Column is defined by usage. */
final class SchemaValueType[SchemaOrColumn, Value]

object SchemaValueType:
  sealed trait Aux[SchemaOrColumn]:
    type Value

  class AuxImpl[SchemaOrColumn, V] extends Aux[SchemaOrColumn]:
    type Value = V
  inline given [SchemaOrColumn, V](using SchemaValueType[SchemaOrColumn, V]): Aux[SchemaOrColumn] =
    new AuxImpl[SchemaOrColumn, V]

  object Aux:
    inline def apply[SchemaOrColumn](using aux: Aux[SchemaOrColumn]): Aux[SchemaOrColumn] = aux
