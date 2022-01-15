package ru.primetalk.typed.ontology.example3

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta3._
import ru.primetalk.typed.ontology.Record
import compiletime.constValue
import compiletime.ops.int._

class Order2Spec:
  import RecordSchema.#:

  object Product extends TableBuilder:
    object id   extends column[Int]
    object name extends column[String]
    type TableSchema = id.type #: name.type #: EmptySchema
    val tableSchema = fields2(id, name)

  object Order extends TableBuilder:
    object id   extends column[Int]
    type TableSchema = id.type #: EmptySchema
    val tableSchema  = fields1(id)

  object OrderItem extends TableBuilder:
    object id        extends column[Int]
    object orderId   extends column[Int]
    object productId extends column[Int]

    type TableSchema = id.type #: orderId.type #: productId.type #: EmptySchema
    val tableSchema  = fields3(id, orderId, productId)

    val orderIdFk   = orderId.foreignKey(Order.id)
    val productIdFk = productId.foreignKey(Product.id)


  @Test def myProductTest = 
    println(Product.id.toString)
    assert(Product.id.toString == "id: int")

  val product1: Product.Row = (1, "product1")
  val product2: Product.Row = (2, "product2")
  val products = Product.relation0(List(product1, product2))
  val order1: Order.Row = Tuple1(1)
  val orders = Order.relation0(List(order1))
  val orderItem1: OrderItem.Row = (1,1,1)
  val orderItem2: OrderItem.Row = (2,1,1)
  val orderItem3: OrderItem.Row = (3,1,2)
  val orderItems = OrderItem.relation0(List(orderItem1,orderItem2, orderItem3))

  @Test def indicesTest =
    type T = OrderItem.tableSchema.IndicesOfProps[OrderItem.TableSchema]
    val res = OrderItem.tableSchema.indicesOfProps(OrderItem.tableSchema)//EmptySchema)
    assert(res == (0,1,2))

  @Test def schema1Test =
    val joinSchema = OrderItem.tableSchema.concat(Order.tableSchema)
    // val result = leftInnerJoin2(orderItems, orders, OrderItem.orderIdFk)
    val cjs = JointSchema(OrderItem.tableSchema, Order.tableSchema)(joinSchema)
    // val result = JointSchema(OrderItem.tableSchema, Order.tableSchema)(joinSchema)
    //   .join(OrderItem.tableSchema, Order.tableSchema)
    // val result = cjs
    //   .leftInnerJoin(OrderItem.orderIdFk)(orderItems.values, orders.values)
      
  // //     // orderItems.withFk(OrderItem.orderIdFk).join(orders)

  //   println(result.mkString("\n"))
    val expected = List(
      orderItem1 ++ order1,
      orderItem2 ++ order1,
      orderItem3 ++ order1,
    )
    
  //   assert(result == expected)

  // @Test def productTest = 
  //   println("productTest starting")
  //   val result = orderItems.withFk(OrderItem.productIdFk).join(products)

  //   println(result.mkString("\n"))
  //   val expected = List(
  //     orderItem1 ++ product1,
  //     orderItem2 ++ product1,
  //     orderItem3 ++ product2,
  //   )
    
  //   assert(result == expected)
