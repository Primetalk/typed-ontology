package ru.primetalk.typed.ontology.example1

import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import java.time.LocalDateTime

object Product extends TableBuilder:
  object id    extends column[Int]
  object name  extends column[String]
  object price extends column[BigInt]

  type TableSchema = id.type #: name.type #: price.type #: EmptySchema
  val tableSchema: TableSchema = fields(id, name, price)
  val idNameSchema             = fields(id, name)
  val primaryKeySchema         = fields(id)

  val fullSchema = infer[TableSchema]

object Order extends TableBuilder:
  object id   extends column[Int]
  object date extends column[LocalDateTime]
  type TableSchema = id.type #: date.type #: EmptySchema
  val tableSchema: TableSchema = fields(id, date)
  val ts = fields(id, date)
  type TS = ts.Type

object OrderItem extends TableBuilder:
  object id        extends column[Int]
  object orderId   extends column[Int]
  object productId extends column[Int]
  // val productId = Product.id// does not work well with toString

  type TableSchema = id.type #: orderId.type #: productId.type #: EmptySchema
  val tableSchema = infer[TableSchema]
  // val tableSchema: TableSchema = id #: orderId #: productId #: EmptySchema

  type SmallerSchema = id.type #: orderId.type #: EmptySchema
  val smallerSchema: SmallerSchema = infer[SmallerSchema]

  lazy val orderIdFk   = orderId.foreignKey(Order.id)
  lazy val productIdFk = productId.foreignKey(Product.id)
