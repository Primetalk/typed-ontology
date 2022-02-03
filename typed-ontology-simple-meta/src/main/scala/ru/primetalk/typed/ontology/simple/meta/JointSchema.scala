package ru.primetalk.typed.ontology.simple.meta

import ru.primetalk.typed.ontology.Record

case class JointSchema[S1 <: RecordSchema,
  S2 <: RecordSchema
  ](schema1: S1, schema2: S2)(val joinSchema: RecordSchema.Concat[S1, S2]):
  transparent inline def concatValues(inline d1: schema1.Values, inline d2: schema2.Values): joinSchema.Values =
    (d1 ++ d2).asInstanceOf[joinSchema.Values]
    
  transparent inline def leftInnerJoin[FK <: ForeignKeyId0](inline fk: FK)(data1: List[schema1.Values], data2: List[schema2.Values]): List[joinSchema.Values] = 
    for
      el1 <- data1
      el2 <- data2
      if schema1.get(fk.left)(el1) == schema2.get(fk.right)(el2)
    yield
      concatValues(el1, el2)

object JointSchema:
  transparent inline def join[
    S1 <: RecordSchema,
    S2 <: RecordSchema
  ](inline schema1: S1, inline schema2: S2) =
    val joinSchema = schema1.concat(schema2)
    JointSchema(schema1, schema2)(joinSchema)
