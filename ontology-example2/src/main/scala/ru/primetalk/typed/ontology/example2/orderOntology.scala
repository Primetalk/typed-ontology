package ru.primetalk.typed.ontology.example2

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

object Product:
  case object id extends TableColumn["id", Int]
  type id = id.type
  case object name extends TableColumn["name", String]
  type name = name.type
  case object description extends TableColumn["description", String]
  type description = description.type
  case object price extends TableColumn["price", BigInt]
  type price           = price.type
  type PriceSchema     = price *: EmptyTuple
  type NamePriceSchema = name *: price *: EmptyTuple
  type DescPriceSchema = Replace[NamePriceSchema, name, description]
  type TableSchema     = id *: name *: price *: EmptyTuple
  val tableSchema: TableSchema = (id, name, price)
  val idNameSchema             = (id, name)
  val namePriceSchema          = name *: price *: EmptyTuple
  val descPriceSchema =
    replace[NamePriceSchema, name, description](name *: price *: EmptyTuple, name, description)
  type priceSchema       = price *: EmptyTuple
  type primitivePriceRow = BigInt *: EmptyTuple
  val priceSchema = price *: EmptyTuple
  type PrimaryKeySchema = id *: EmptyTuple
  val primaryKeySchema: PrimaryKeySchema = Tuple1(id)

  val fullSchema = RecordSchema.infer[TableSchema]
  val svt        = summon[SchemaValueType[TableSchema, ?]]
  type Row          = svt.Value
  type PrimitiveRow = (Int, String, BigInt)
  summon[Row =:= RecordTupleValue[TableSchema, (Int, String, BigInt)]]
  summon[(Int, String, BigInt) <:< Row]

object Order:
  case object id extends TableColumn["id", Int]
  type id = id.type
  case object date extends TableColumn["date", LocalDateTime]
  type date        = date.type
  type TableSchema = id *: date *: EmptyTuple
  val tableSchema: TableSchema = (id, date)
  val ts                       = (id, date)

  val svt = SchemaValueType.Aux[TableSchema]
  type Row = svt.Value

object OrderItem:
  object id extends TableColumn["id", Int]
  type id = id.type
  object orderId extends TableColumn["orderId", Int]
  type orderId = orderId.type
  object productId extends TableColumn["productId", Int]
  type productId   = productId.type
  type TableSchema = id *: orderId *: productId *: EmptyTuple
  val tableSchema: TableSchema  = (id, orderId, productId)
  val tableSchema2: TableSchema = RecordSchema.infer[TableSchema]
  val tableSchema3: TableSchema = id *: orderId *: productId *: EmptyTuple

  type SmallerSchema = id *: orderId *: EmptyTuple
  val smallerSchema: SmallerSchema = RecordSchema.infer[SmallerSchema]
//
//  lazy val orderIdFk   = orderId.foreignKey(Order.id)
//  lazy val productIdFk = productId.foreignKey(Product.id)
  val svt = SchemaValueType.Aux[TableSchema]
  type Row = svt.Value

object Address:
  object street extends TableColumn["street", String]
  type street = street.type
  object building extends TableColumn["building", Int]
  type building    = building.type
  type TableSchema = street *: building *: EmptyTuple
  val tableSchema: TableSchema = street *: building *: EmptyTuple
  type Value = ValueWithSchema[TableSchema, (String, Int)]
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
