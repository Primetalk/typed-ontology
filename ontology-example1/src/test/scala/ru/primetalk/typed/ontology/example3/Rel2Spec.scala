package ru.primetalk.typed.ontology.example3

import ru.primetalk.typed.ontology.example1._
import java.time.LocalDateTime
import cats.instances._
import ru.primetalk.typed.ontology.simplemeta.Relation2Meta
import ru.primetalk.typed.ontology.simplemeta.SimplePropertyId

trait TestDataRel2 extends BaseSpec:
  val product1: Product.Row = (1, "product1")
  val product2: Product.Row = (2, "product2")
  val products = Product.relation2(List(product1, product2))
  val order1: Order.Row = (1, LocalDateTime.of(2022, java.time.Month.JANUARY, 23, 0, 0, 0, 0))
  val orders = Order.relation2(List(order1))
  val orderItem1: OrderItem.Row = (1,1,1)
  val orderItem2: OrderItem.Row = (2,1,1)
  val orderItem3: OrderItem.Row = (3,1,2)
  val orderItems = OrderItem.relation2(List(orderItem1,orderItem2, orderItem3))

class Rel2Spec extends TestDataRel2:

  test("Property to string"){
    println(Product.id.toString)
    assertResult("id: int")(Product.id.toString)
  }
  test("projection"){
    val ids = products.projection(Product.primaryKeySchema)
    ids.rows should equal(List(Tuple(1), Tuple(2)))
    ids.schema.toString should equal(Product.primaryKeySchema.toString)
  }
  test("schema concat"){
    val schema3 = products.schema.concat(orderItems.schema)
    schema3.toString should equal(products.schema.toString + ", " + orderItems.schema.toString)
  }
  test("cross product from"){
    val poi = products.crossProductFrom(orderItems)
    poi.rows should equal(List(
      (1,1,1,1,"product1"), 
      (1,1,1,2,"product2"), 
      (2,1,1,1,"product1"), 
      (2,1,1,2,"product2"), 
      (3,1,2,1,"product1"), 
      (3,1,2,2,"product2")
    ))
  }
  test("cross product"){
    val poi = orderItems.crossProduct(products)
    poi.rows should equal(List(
      (1,1,1,1,"product1"), 
      (1,1,1,2,"product2"), 
      (2,1,1,1,"product1"), 
      (2,1,1,2,"product2"), 
      (3,1,2,1,"product1"), 
      (3,1,2,2,"product2")
    ))
  }
  test("Calculate column"){
    object price extends OrderItem.column[Long]
    val idGetter = orderItems.schema.propertyGetter(OrderItem.id)
    val p = orderItems.prependCalcColumn(price)(idGetter(_) * 10L)
    p.show should equal(
      """price: long, id: int, orderId: int, productId: int
        |-----
        |(10,1,1,1)
        |(20,2,1,1)
        |(30,3,1,2)""".stripMargin
    )
  }
  test("Rename column"){
    object id2 extends OrderItem.column[Int]
    val p = orderItems.rename(OrderItem.id, id2)
    p.show should equal(
      """id2: int, orderId: int, productId: int
        |-----
        |(1,1,1)
        |(2,1,1)
        |(3,1,2)""".stripMargin
    )
  }
