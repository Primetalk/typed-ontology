package ru.primetalk.typed.ontology.example3

import org.junit.Test
import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import compiletime.constValue
import compiletime.constValueTuple
import compiletime.ops.int._
import ru.primetalk.typed.ontology.example1._
import java.time.LocalDateTime
import ru.primetalk.typed.ontology.simple.listrelation._

trait TestData extends BaseSpec:
  val product1: Product.Row = (1, "product1", BigInt(10))
  val product2: Product.Row = (2, "product2", BigInt(20))
  val products = RelationList(Product.tableSchema)(List(product1, product2))
  val order1: Order.Row = (1, LocalDateTime.of(2022, java.time.Month.JANUARY, 23, 0, 0, 0, 0))
  val orders = RelationList(Order.tableSchema)(List(order1))
  val orderItem1: OrderItem.Row = (1,1,1)
  val orderItem2: OrderItem.Row = (2,1,1)
  val orderItem3: OrderItem.Row = (3,1,2)
  val orderItems = RelationList(OrderItem.tableSchema)(List(orderItem1,orderItem2, orderItem3))

class Order2Spec extends TestData:

  test("Property to string"){
    println(Product.id.toString)
    assertResult("id: int")(Product.id.toString)
  }
  test("Projection to smaller schema"){
    val v = orderItems.projection(OrderItem.smallerSchema)
    v.rows should equal(List((1,1), (2,1), (3,1)))
  }
  test("Schema concatenation"){
    val joinSchema = OrderItem.tableSchema.concat(Order.tableSchema)
    OrderItem.tableSchema.toString should equal("id: int, orderId: int, productId: int")
    Order.tableSchema.toString should equal("id: int, date: LocalDateTime")
    joinSchema.toString should equal(OrderItem.tableSchema.toString + ", " + Order.tableSchema.toString)
  }
  test("Join using foreign key"){
    val result = fullInnerJoin(orderItems, orders, OrderItem.orderIdFk)
    // val cjs = JointSchema(OrderItem.tableSchema, Order.tableSchema)(joinSchema)
   
    val expected = List(
      orderItem1 ++ order1,
      orderItem2 ++ order1,
      orderItem3 ++ order1,
    )
    
    assertResult(expected)(result)
  }
  test("Cross product"){
    val result = crossProduct(orderItems, orders)
    val expected = List(
      orderItem1 ++ order1,
      orderItem2 ++ order1,
      orderItem3 ++ order1,
    )
    result should equal(expected)
  }
  // test("Join using withFk"){
  //   val result = orderItems.withFk(OrderItem.productIdFk).join(products)

  //   val expected = List(
  //     orderItem1 ++ product1,
  //     orderItem2 ++ product1,
  //     orderItem3 ++ product2,
  //   )
    
  //   assertResult(expected)(result)
  // }
  test("Indices of properties"){
    type T = OrderItem.tableSchema.IndicesOfProps[OrderItem.TableSchema]
    val res = OrderItem.tableSchema.indicesOfProps(OrderItem.tableSchema)
    assert(res == (0,1,2))
    val res2 = OrderItem.tableSchema.indicesOfProps(OrderItem.smallerSchema)
    assert(res2 == (0,1))
  }

  test("Projection using indices"){
    type Iid = orderItems.schema.IndexOfProp[OrderItem.id.type]
    type IInt = RecordSchema.IndexOfTypeInTuple[(Int, String), Int]
    val iid: IInt = 0
    type IorderId = orderItems.schema.IndexOfProp[OrderItem.orderId.type]
    type Inds = orderItems.schema.IndicesOfProps[OrderItem.smallerSchema.type]
    val indicesU = orderItems.schema.indicesOfProps(OrderItem.smallerSchema)
    val indices1: (Int, Int) = indicesU
    val indices2: (Iid, IorderId) = indicesU
    val indices: orderItems.schema.IndicesOfProps[OrderItem.smallerSchema.type] = indicesU

    val p = Product.primaryKeySchema.projectorFrom(Product.tableSchema)
    val res = products.rows.map(p)
    assert(res == List(Tuple1(1), Tuple1(2)), s"res=$res")
  }

  test("propertyGetter"){
    val getId = orderItems.schema.propertyGetter(OrderItem.id)
    val res = orderItems.rows.map(getId)
    assert(res == List(1,2,3))
    val getProductId = orderItems.schema.propertyGetter(OrderItem.productId)
    val pres = orderItems.rows.map(getProductId)
    assert(pres == List(1,1,2))
  }
