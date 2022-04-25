package ru.primetalk.typed.ontology.example2

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta2._
import ru.primetalk.typed.ontology.metameta.Record
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
  val products = List(product1, product2)
  val order1: orderProps.baseSchema.Values = Tuple1(1)
  val orders = List(order1)
  val orderItem1: orderItemProps.baseSchema.Values = (1,1,1)
  val orderItem2: orderItemProps.baseSchema.Values = (2,1,1)
  val orderItem3: orderItemProps.baseSchema.Values = (3,1,2)
  val orderItems = List(orderItem1,orderItem2)

  @Test def schemaTest =
    val t1 = (1, "")
    val t2 = (1, "", 2.0)
    val t3 = t1 ++ t2
    // println(productProps.a.name)
    // val a: productProps.s.Values = (1, "")
    // println('{Tuple1(1)}.show)
    val joinSchema = orderItemProps.baseSchema.concat(orderProps.baseSchema)
    println(s"jointSchema ${joinSchema.toString}")
    val result = JointSchema.join(orderItemProps.baseSchema, orderProps.baseSchema)
      .leftInnerJoin(orderItemProps.orderIdFk)(orderItems, orders)

    println(result.mkString("\n"))
    val expected = List(
      orderItem1 ++ order1,
      orderItem2 ++ order1,
    )
    println(expected.mkString("\n"))
    
    assert(result == expected)

  @Test def productTest = 
    val joinSchema = orderItemProps.baseSchema.concat(productProps.baseSchema)
    println(s"jointSchema2 ${joinSchema.toString}")
    val result = JointSchema.join(orderItemProps.baseSchema, productProps.baseSchema)
      .leftInnerJoin(orderItemProps.productIdFk)(orderItem3 :: orderItems, products)

    println(result.mkString("\n"))
    val expected = List(
      orderItem3 ++ product2,
      orderItem1 ++ product1,
      orderItem2 ++ product1,
    )
    
    assert(result == expected)
