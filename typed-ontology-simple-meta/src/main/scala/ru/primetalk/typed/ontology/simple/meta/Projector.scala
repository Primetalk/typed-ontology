package ru.primetalk.typed.ontology.simple.meta

trait Projector[From <: SchemaLike, To <: SchemaLike]:
  val from: SchemaValueType.Aux1[From]
  val to: SchemaValueType.Aux1[To]
  def apply(v: from.Value): to.Value

trait Concatenator[A <: RecordSchema, B <: RecordSchema]:
  val aSvt: RecordSchemaValueType[A]
  val bSvt: RecordSchemaValueType[B]
  val abSvt: RecordSchemaValueType[RecordSchema.Concat[A, B]]
  def apply(a: aSvt.Value, b: bSvt.Value): abSvt.Value
