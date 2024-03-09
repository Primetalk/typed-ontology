package ru.primetalk.typed.ontology.dbquill


import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import java.time.LocalDateTime
import SimpleTypes.{given, *}

object Order extends TableBuilder:
  object id   extends column[Int]
  val id1 = id
  type Id = id1.type
  object date extends column[LocalDateTime]
  type Date = date.type
  type TableSchema = Id #: Date #: EmptySchema
  val tableSchema: TableSchema = fields(id, date)
  val ts = fields(id, date)
  type TS = ts.Type
  val svt = summon[SchemaValueType.Aux1[TableSchema]]
  type Row = svt.Value
