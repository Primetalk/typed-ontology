package ru.primetalk.typed.ontology.simple.meta

trait Projector[From <: SchemaLike, VFrom, To <: SchemaLike, VTo]:
  val from: SchemaValueType[From, VFrom]
  val to: SchemaValueType[To, VTo]
  def apply(v: VFrom): VTo

trait Concatenator[A <: RecordSchema, VA, B <: RecordSchema, VB, VAB]:
  val aSvt: SchemaValueType[A, VA]
  val bSvt: SchemaValueType[B, VB]
  type Schema = RecordSchema.Concat[A, B]
  def schemaConcat(a: A, b: B): Schema // r1.schema.appendOtherSchema(schema)
  val abSvt: SchemaValueType[Schema, VAB]
  def apply(a: VA, b: VB): VAB
