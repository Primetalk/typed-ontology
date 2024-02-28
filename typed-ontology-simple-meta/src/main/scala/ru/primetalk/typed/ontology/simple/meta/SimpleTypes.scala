package ru.primetalk.typed.ontology.simple.meta

object SimpleTypes:
  given [T]: SchemaValueType[ScalarSchema1[T]] = 
    new:
      type Value = T
  given SchemaTupleValueType[EmptyTupleSchema.type] =
    new:
      type Value = EmptyTuple.type
  given [HS <: SchemaLike, TS <: TupleSchema](using hs: SchemaValueType[HS], ts: SchemaTupleValueType[TS]): SchemaTupleValueType[NonEmptyTupleSchema[HS, TS]] =
    new:
      type Value = hs.Value *: ts.Value
