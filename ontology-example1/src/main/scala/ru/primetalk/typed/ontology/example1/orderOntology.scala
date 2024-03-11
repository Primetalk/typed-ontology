package ru.primetalk.typed.ontology.example1

import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import java.time.LocalDateTime
import SimpleTypes.{given, *}

object Product extends TableBuilder:
  object id    extends column[Int]
  val id1 = id
  type Id = id1.type 
  //type Id = id.type // Scala 3 bug
  object name  extends column[String]
  val name1 = name
  type Name = name1.type
  object price extends column[BigInt]
  type Price = price.type
  type PriceSchema = Price #: EmptySchema
  type NamePriceSchema = Name #: Price #: EmptySchema
  type TableSchema = Id #: Name #: Price #: EmptySchema
  val tableSchema: TableSchema = fields(id, name, price)
  val idNameSchema             = fields(id, name)
  val namePriceSchema             = fields(name, price)
  type PrimaryKeySchema        = id.type #: EmptySchema
  val primaryKeySchema: PrimaryKeySchema         = fields(id)

  val fullSchema = infer[TableSchema]
  val svt = summon[SchemaValueType.Aux1[TableSchema]]
  type Row = svt.Value

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

object OrderItem extends TableBuilder:
  object id        extends column[Int]
  val id1 = id
  type Id = id1.type
  object orderId   extends column[Int]
  type OrderId = orderId.type
  object productId extends column[Int]
  type ProductId = productId.type
  // val productId = Product.id// does not work well with toString

  type TableSchema = Id #: OrderId #: ProductId #: EmptySchema
 
  val tableSchema: TableSchema = fields(id, orderId, productId)
  // val tableSchema = infer[TableSchema]
  // val tableSchema: TableSchema = id #: orderId #: productId #: EmptySchema

  type SmallerSchema = Id #: OrderId #: EmptySchema
  val smallerSchema: SmallerSchema = infer[SmallerSchema]

  lazy val orderIdFk   = orderId.foreignKey(Order.id)
  lazy val productIdFk = productId.foreignKey(Product.id)
  val svt = summon[SchemaValueType.Aux1[TableSchema]]
  type Row = svt.Value
