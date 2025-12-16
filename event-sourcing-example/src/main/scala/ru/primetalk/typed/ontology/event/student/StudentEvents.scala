package ru.primetalk.typed.ontology.event.student

import ru.primetalk.typed.ontology.event.ontology.EventOntology

object StudentEvents {

  val svt = summon[SchemaValueType[TableSchema, ?]]
  type Row = svt.Value

  val events = new EventOntology[
    EmptyTuple,
    EmptyTuple,
    Student.PrimaryKeySchema,
    PrimaryKeyValue <: Tuple,
    EntitySchema <: Tuple,
    EntityValue <: Tuple
  ](
    customAttributes: CustomAttributesSchema,
    primaryKeySchema: PrimaryKeySchema,
    entitySchema: EntitySchema,
  )
}
