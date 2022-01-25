package ru.primetalk.typed.ontology.example1

import ru.primetalk.typed.ontology.simplemeta._
import ru.primetalk.typed.ontology.Record
import java.time.LocalDateTime

object Product extends TableBuilder:
  object id   extends column[Int]
  object name extends column[String]
  type TableSchema = id.type #: name.type #: EmptySchema
  val tableSchema: TableSchema = fields(id, name)
  val primaryKeySchema = fields(id)

object Order extends TableBuilder:
  object id   extends column[Int]
  object date extends column[LocalDateTime]
  type TableSchema = id.type #: date.type #: EmptySchema
  val tableSchema: TableSchema  = fields(id, date)

object OrderItem extends TableBuilder:
  object id        extends column[Int]
  object orderId   extends column[Int]
  object productId extends column[Int]

  type TableSchema = id.type #: orderId.type #: productId.type #: EmptySchema
  // val tableSchema: TableSchema = RecordSchema.constSchema[TableSchema]
  val tableSchema = tupleToSchema((id, orderId, productId))
  // val tableSchema: TableSchema = id #: orderId #: productId #: EmptySchema 

  type SmallerSchema = id.type #: orderId.type #: EmptySchema
  val smallerSchema: SmallerSchema = id #: orderId #: EmptySchema

  lazy val orderIdFk   = orderId.foreignKey(Order.id)
  lazy val productIdFk = productId.foreignKey(Product.id)
