package ru.primetalk.typed.ontology.example3

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta3._
import ru.primetalk.typed.ontology.Record
import compiletime.constValue
import compiletime.ops.int._

class OrderSpec:

  abstract final class Product
  abstract final class Order
  abstract final class OrderItem

  object productProps extends RecordSchemaBuilder[Product]:
    val id = property[Int]("id")
    val name = property[String]("name")
    val baseSchema = fields2(id, name)
    // val s = tupleToSchema((id, name))
    println(s"s=${showExpr((id, name))}")
    val a = traitExpr("name")

  object orderProps extends RecordSchemaBuilder[Order]:
    val id = property[Int]("id")
    val baseSchema = fields1(id)
    // val baseSchema = emptySchema.prepend(id)// fields(id)

  object orderItemProps extends RecordSchemaBuilder[OrderItem]:
    val id = property[Int]("id")
    val orderId = property[Int]("orderId")
    val productId = property[Int]("productId")
    val orderIdFk = orderId.foreignKey(orderProps.id)
    val productIdFk = productId.foreignKey(productProps.id)
    val baseSchema = fields3(id, orderId, productId)

  val product1: productProps.baseSchema.Values = (1, "product1")
  val product2: productProps.baseSchema.Values = (2, "product2")
  val products = Relation0.apply(productProps.baseSchema)(List(product1, product2))
  val order1: orderProps.baseSchema.Values = Tuple1(1)
  val orders = Relation0.apply(orderProps.baseSchema)(List(order1))
  val orderItem1: orderItemProps.baseSchema.Values = (1,1,1)
  val orderItem2: orderItemProps.baseSchema.Values = (2,1,1)
  val orderItem3: orderItemProps.baseSchema.Values = (3,1,2)
  val orderItems = Relation0.apply(orderItemProps.baseSchema)(List(orderItem1,orderItem2, orderItem3))

  @Test def schemaTest =
    val result = orderItems.withFk(orderItemProps.orderIdFk).join(orders)

    println(result.mkString("\n"))
    val expected = List(
      orderItem1 ++ order1,
      orderItem2 ++ order1,
    )
    
    assert(result == expected)

  @Test def productTest = 
    println("productTest starting")
    val result = orderItems.withFk(orderItemProps.productIdFk).join(products)

    println(result.mkString("\n"))
    val expected = List(
      orderItem1 ++ product1,
      orderItem2 ++ product1,
      orderItem3 ++ product2,
    )
    
    assert(result == expected)
