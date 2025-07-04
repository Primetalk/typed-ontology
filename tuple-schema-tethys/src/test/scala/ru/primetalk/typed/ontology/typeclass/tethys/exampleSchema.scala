package ru.primetalk.typed.ontology.typeclass.tethys

import scala.language.experimental.namedTuples
import ru.primetalk.typed.ontology.typeclass.schema.{
  Column,
  RecordSchema,
  RecordTupleValue,
  RecordValueType,
  Replace,
  SchemaValueType,
  replace
}
import ru.primetalk.typed.ontology.typeclass.table.TableColumn
import ru.primetalk.typed.ontology.typeclass.table.TableColumn.given
import ru.primetalk.typed.ontology.typeclass.schema.RecordTupleValue.{*, given}

import java.time.LocalDateTime
import ru.primetalk.typed.ontology.typeclass.schema.ValueWithSchema

object Address:
  object street extends TableColumn["street", String]
  type street = street.type
  object building extends TableColumn["building", Int]
  type building    = building.type
  type TableSchema = street *: building *: EmptyTuple
  val tableSchema: TableSchema = street *: building *: EmptyTuple
  type ValueSchema = String *: Int *: EmptyTuple
  type Value       = ValueWithSchema[TableSchema, ValueSchema]
  val svt = summon[SchemaValueType[TableSchema, ?]]
  type Row = svt.Value

object Person:
  case object name extends TableColumn["name", String]
  type name = name.type
  case object address extends TableColumn["address", Address.Row]
  type address     = address.type
  type TableSchema = name *: address *: EmptyTuple
  val tableSchema: TableSchema = name *: address *: EmptyTuple

  val svt = summon[SchemaValueType[TableSchema, ?]]
  // val svt = SchemaValueType.Aux[TableSchema]
  type Row = svt.Value
