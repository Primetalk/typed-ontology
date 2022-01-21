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
    val primaryKeySchema = fields1(id)

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
  object products extends Product.Relation1{ val values = List(product1, product2)}
  val order1: Order.Row = Tuple1(1)
  object orders extends Order.Relation1{ val values = List(order1) }
  val orderItem1: OrderItem.Row = (1,1,1)
  val orderItem2: OrderItem.Row = (2,1,1)
  val orderItem3: OrderItem.Row = (3,1,2)
  object orderItems extends OrderItem.Relation1 {
    val values = List(orderItem1,orderItem2, orderItem3)
  }

  val smallerSchema = EmptySchema
    .prepend(OrderItem.orderId)
    .prepend(OrderItem.id)

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

  @Test def indicesTest =
    type T = OrderItem.tableSchema.IndicesOfProps[OrderItem.TableSchema]
    val res = OrderItem.tableSchema.indicesOfProps(OrderItem.tableSchema)
    assert(res == (0,1,2))
    val res2 = OrderItem.tableSchema.indicesOfProps(smallerSchema)
    assert(res2 == (0,1))

  @Test def projectionTest =
    type Iid = orderItems.schema.IndexOfProp[OrderItem.id.type]
    type IInt = RecordSchema.IndexOfTypeInTuple[(Int, String), Int]
    // val iid: IInt = 0 
    type IorderId = orderItems.schema.IndexOfProp[OrderItem.orderId.type]
    type Inds = orderItems.schema.IndicesOfProps[smallerSchema.type]
    val indicesU = orderItems.schema.indicesOfProps(smallerSchema)
    val indices1: (Int, Int) = indicesU
    val indices2: (Iid, IorderId) = indicesU
    val indices: orderItems.schema.IndicesOfProps[smallerSchema.type] = indicesU

    val p = Product.primaryKeySchema.projectorFrom(Product.tableSchema)
    val res = products.values.map(p)
    // val projected = orderItems.schema.projection(smallerSchema)
    assert(res == List(Tuple1(1), Tuple1(2)), s"res=$res")


  @Test def propGetTest =
    val getId = orderItems.schema.propertyGetter(OrderItem.id)
    val res = orderItems.values.map(getId)
    assert(res == List(1,2,3))
    val getProductId = orderItems.schema.propertyGetter(OrderItem.productId)
    val pres = orderItems.values.map(getProductId)
    assert(pres == List(1,1,2))
