package ru.primetalk.typed.ontology.simple.sqlrelation

import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.relalg.ExprClassicDsl
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType

sealed trait SqlRelation[S <: RecordSchema, VS] extends ExprClassicDsl:
  self =>

  val schema: S
  type Schema = S

  val svt: SchemaValueType[S, VS]

  type Row = VS

final case class EntityRelation[S <: RecordSchema, VS](name: String, schema: S, svt: SchemaValueType[S, VS]) extends SqlRelation[S, VS]

def sql[S<:RecordSchema, VS](r: SqlRelation[S, VS]): String =
  r match
    case EntityRelation(name, schema, svt) =>
      s"SELECT ${} FROM $name"
  