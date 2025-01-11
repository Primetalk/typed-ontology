package ru.primetalk.typed.ontology.example2

import ru.primetalk.typed.ontology.typeclass.schema.{Column, RecordSchema}
import ru.primetalk.typed.ontology.typeclass.table.TableColumn
import ru.primetalk.typed.ontology.typeclass.table.TableColumn.given

import java.time.LocalDateTime

object Product:
  object id    extends TableColumn["id", Int]
  type id = id.type 
  object name  extends TableColumn["name", String]
  type name = name.type
  object price extends TableColumn["price", BigInt]
  type price = price.type
  type PriceSchema = price *: EmptyTuple
  type NamePriceSchema = name *: price *: EmptyTuple
  type TableSchema = id *: name *: price *: EmptyTuple
  val tableSchema: TableSchema = (id, name, price)
  val idNameSchema             = (id, name)
  val namePriceSchema             = (name, price)
  type PrimaryKeySchema        = id *: EmptyTuple
  val primaryKeySchema: PrimaryKeySchema         = Tuple1(id)

  val fullSchema = RecordSchema.infer[TableSchema]
//  val svt = summon[SchemaValueType.Aux1[TableSchema]]
//  type Row = svt.Value

object Order:
  object id   extends TableColumn["id", Int]
  type id = id.type
  object date extends TableColumn["date", LocalDateTime]
  type date = date.type
  type TableSchema = id *: date *: EmptyTuple
  val tableSchema: TableSchema = (id, date)
  val ts = (id, date)
//  type TS = ts.Type
//  val svt = summon[SchemaValueType.Aux1[TableSchema]]
//  type Row = svt.Value

object OrderItem:
  object id        extends TableColumn["id", Int]
  type id = id.type
  object orderId   extends TableColumn["orderId", Int]
  type orderId = orderId.type
  object productId extends TableColumn["productId", Int]
  type productId = productId.type
  type TableSchema = id *: orderId *: productId *: EmptyTuple
  val tableSchema: TableSchema = (id, orderId, productId)
  val tableSchema2: TableSchema = RecordSchema.infer[TableSchema]
   val tableSchema3: TableSchema = id *: orderId *: productId *: EmptyTuple

  type SmallerSchema = id *: orderId *: EmptyTuple
  val smallerSchema: SmallerSchema = RecordSchema.infer[SmallerSchema]
//
//  lazy val orderIdFk   = orderId.foreignKey(Order.id)
//  lazy val productIdFk = productId.foreignKey(Product.id)
//  val svt = summon[SchemaValueType.Aux1[TableSchema]]
//  type Row = svt.Value
