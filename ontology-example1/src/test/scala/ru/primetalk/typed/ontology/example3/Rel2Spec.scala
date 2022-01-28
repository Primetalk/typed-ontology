package ru.primetalk.typed.ontology.example3

import ru.primetalk.typed.ontology.example1._
import java.time.LocalDateTime
import cats.instances._
import ru.primetalk.typed.ontology.simplemeta.#:
import ru.primetalk.typed.ontology.simplemeta.EmptySchema
import ru.primetalk.typed.ontology.simplemeta.Relation2Meta
import ru.primetalk.typed.ontology.simplemeta.SimplePropertyId
import ru.primetalk.typed.ontology.simplemeta.RecordSchema

trait TestDataRel2 extends BaseSpec:
  val product1: Product.Row = (1, "product1", BigInt(10))
  val product2: Product.Row = (2, "product2", BigInt(20))
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
      (1,1,1,1,"product1", BigInt(10)), 
      (1,1,1,2,"product2", BigInt(20)), 
      (2,1,1,1,"product1", BigInt(10)), 
      (2,1,1,2,"product2", BigInt(20)), 
      (3,1,2,1,"product1", BigInt(10)), 
      (3,1,2,2,"product2", BigInt(20)),
    ))
  }
  test("ValueOf"){
    summon[ValueOf[Product.name.type]].value should equal(Product.name)
  }
  test("constSchema"){
    type S1 =  Product.name.type #: Product.price.type #: EmptySchema
    val s2 = Product.fields(Product.name, Product.price)
    val s1: S1 = RecordSchema.constSchema[S1]
    s1 should equal(s2)
  }
  test("Remove property from schema"){
    val  s1 = Product.tableSchema.remove(Product.price)
    val s2 = Product.id #: Product.name #: EmptySchema
    s1 should equal(s2)
  }
  test("cross product"){
    val poi = orderItems.crossProduct(products)
    val s = OrderItem.tableSchema.concat(Product.tableSchema.remove(Product.price))// Product.id #: Product.name #: EmptySchema)
    // val s = OrderItem.tableSchema.concat(Product.idNameSchema)// Product.id #: Product.name #: EmptySchema)
    val res = poi.projection(s)
    res.rows should equal(List(
      (1,1,1,1,"product1"), 
      (1,1,1,2,"product2"), 
      (2,1,1,1,"product1"), 
      (2,1,1,2,"product2"), 
      (3,1,2,1,"product1"), 
      (3,1,2,2,"product2"),
    ))
  }
  test("Calculate column"){
    object price extends OrderItem.column[Long]
    val idGetter = orderItems.schema.propertyGetter(OrderItem.id)
    val p = orderItems.prependCalcColumn(price)(idGetter(_) * 10L)
    val s = price #: OrderItem.tableSchema
    val res = p.projection(s)
    res.show should equal(
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
  test("Filter"){
    val idGetter = orderItems.schema.propertyGetter(OrderItem.id)
    object id2 extends OrderItem.column[Int]
    val p = orderItems.filter(idGetter(_) == 1)
    p.rows should equal(List(orderItem1))
  }
  test("Union"){
    val p = orderItems ++ orderItems
    p.rows.size should equal( orderItems.rows.size * 2)
  }

  test("empty"){
    val p = Relation2Meta.empty[OrderItem.tableSchema.type, List](OrderItem.tableSchema)
    p.rows shouldBe empty
  }

  test("Expenses report"){

  }
    