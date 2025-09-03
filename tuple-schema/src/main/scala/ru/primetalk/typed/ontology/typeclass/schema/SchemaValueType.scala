package ru.primetalk.typed.ontology.typeclass.schema

import scala.annotation.implicitNotFound

/** Represents value type of schema or column. Whether it's Schema or Column is defined by usage. */
@implicitNotFound(msg = "Cannot prove that value of schema ${SchemaOrColumn} is ${V}.")
final class SchemaValueType[SchemaOrColumn, V]:
  type Value = V

object SchemaValueType:
  sealed trait Aux[SchemaOrColumn]:
    type Value
    val svt: SchemaValueType[SchemaOrColumn, Value]

  final class AuxImpl[SchemaOrColumn, V](val svt: SchemaValueType[SchemaOrColumn, V]) extends Aux[SchemaOrColumn]:
    type Value = V

  inline given [SchemaOrColumn, V](using SchemaValueType[SchemaOrColumn, V]): Aux[SchemaOrColumn] =
    new AuxImpl[SchemaOrColumn, V](summon)

  object Aux:
    inline def apply[SchemaOrColumn](using aux: Aux[SchemaOrColumn]): Aux[SchemaOrColumn] = aux
