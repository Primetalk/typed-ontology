package ru.primetalk.typed.ontology.dbquill


import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import java.time.LocalDateTime
import SimpleTypes.{given, *}
import ru.primetalk.typed.ontology.dbquill.parser.TupleConverter

object Order extends TableBuilder:
  object id   extends column[Int]
  type Id = id.type
  object date extends column[LocalDateTime]
  type Date = date.type
  type TableSchema = Id #: Date #: EmptySchema
  val tableSchema: TableSchema = fields(id, date)
  val ts = fields(id, date)
  type TS = ts.Type
  val svt = summon[SchemaValueType.Aux1[TableSchema]]
  type Row = TupleConverter[svt.Value]
