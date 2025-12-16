package ru.primetalk.typed.ontology.event.student

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

object Student:
  case object id extends TableColumn["id", Int]
  type id = id.type
  case object name extends TableColumn["name", String]
  type name = name.type
  case object credits extends TableColumn["credits", Int]
  type credits = credits.type
  
  type TableSchema = id *: name *: credits *: EmptyTuple
  val tableSchema: TableSchema = (id, name, credits)
  type PrimaryKeySchema = id *: EmptyTuple
  val primaryKeySchema: PrimaryKeySchema = Tuple1(id)

  val svt = summon[SchemaValueType[TableSchema, ?]]
  type Row = svt.Value

  val svtPk = summon[SchemaValueType[PrimaryKeySchema, ?]]
  type RowPk = svtPk.Value

object Course:
  case object id extends TableColumn["id", Int]
  type id = id.type
  case object name extends TableColumn["name", String]
  type name = name.type
  case object credits extends TableColumn["credits", Int]
  type credits = credits.type
  case object curriculum extends TableColumn["curriculum", List[String]]
  type curriculum = curriculum.type
  
  type TableSchema = id *: name *: credits *: curriculum *: EmptyTuple
  val tableSchema: TableSchema = (id, name, credits, curriculum)
  type PrimaryKeySchema = id *: EmptyTuple
  val primaryKeySchema: PrimaryKeySchema = Tuple1(id)

object Enrollment:
  object id extends TableColumn["id", Int]
  type id = id.type
  object studentId extends TableColumn["studentId", Int]
  type studentId = studentId.type
  object courseId extends TableColumn["courseId", Int]
  type courseId   = courseId.type
  type TableSchema = id *: studentId *: courseId *: EmptyTuple
  val tableSchema: TableSchema  = (id, studentId, courseId)
