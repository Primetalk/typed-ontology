package ru.primetalk.typed.ontology.dbquill


import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import java.time.LocalDate
import SimpleTypes.{given, *}
import ru.primetalk.typed.ontology.dbquill.parser.TupleConverter

object Order1 extends TableBuilder:
  object id   extends column[Int]
  val id1 = id
  type Id = id.type
  object date extends column[LocalDate]
  val date1 = date
  type Date = date.type
  type TableSchema = Id #: Date #: EmptySchema
  val tableSchema: TableSchema = fields(id, date)
  val ts = fields(id, date)
  type TS = ts.Type
  val svt = summon[SchemaValueType.Aux1[TableSchema]]
  type Row = TupleConverter[svt.Value]
  // type ARow = svt.Value #@ TableSchema 
  type ARow = (*:[Int, *:[LocalDate, EmptyTuple]]) #@ TableSchema
