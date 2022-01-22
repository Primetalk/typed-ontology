package ru.primetalk.typed.ontology.example1

import ru.primetalk.typed.ontology.simplemeta._
import ru.primetalk.typed.ontology.Record
import java.time.LocalDateTime

object Product extends ru.primetalk.typed.ontology.simplemeta.TableBuilder:
  object id   extends column[Int]
  object name extends column[String]
  type TableSchema = id.type #: name.type #: EmptySchema
  val tableSchema: TableSchema = id #: name #:  EmptySchema
  val primaryKeySchema = id #: EmptySchema

object Order extends ru.primetalk.typed.ontology.simplemeta.TableBuilder:
  object id   extends column[Int]
  object date extends column[LocalDateTime]
  type TableSchema = id.type #: date.type #: EmptySchema
  val tableSchema: TableSchema  = id #: date #: EmptySchema

object OrderItem extends ru.primetalk.typed.ontology.simplemeta.TableBuilder:
  object id        extends column[Int]
  object orderId   extends column[Int]
  object productId extends column[Int]

  type TableSchema = id.type #: orderId.type #: productId.type #: EmptySchema
  val tableSchema: TableSchema = id #: orderId #: productId #: EmptySchema

  type SmallerSchema = id.type #: orderId.type #: EmptySchema
  val smallerSchema: SmallerSchema = id #: orderId #: EmptySchema

  lazy val orderIdFk   = orderId.foreignKey(Order.id)
  lazy val productIdFk = productId.foreignKey(Product.id)
