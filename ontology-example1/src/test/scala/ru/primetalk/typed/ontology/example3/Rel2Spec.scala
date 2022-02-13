package ru.primetalk.typed.ontology.example3

import ru.primetalk.typed.ontology.example1.{Product, Order, OrderItem}
import java.time.LocalDateTime
// import cats.instances._
import ru.primetalk.typed.ontology.simple.meta.#:
import ru.primetalk.typed.ontology.simple.meta.EmptySchema
import ru.primetalk.typed.ontology.simple.relalg.Relation
import ru.primetalk.typed.ontology.simple.relalg.convertSortedMapToRelation
import ru.primetalk.typed.ontology.simple.relalg.convertSortedMapToV
import ru.primetalk.typed.ontology.simple.meta.SimplePropertyId
import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.relalg.relation

import scala.collection.immutable.SortedMap
import cats.MonoidK
import cats.Applicative

trait TestDataRel2 extends BaseSpec:
  val product1: Product.Row = (1, "product1", BigInt(5))
  val product2: Product.Row = (2, "product2", BigInt(20))
  val products = Product.relation(List(product1, product2))
  val order1: Order.Row = (1, LocalDateTime.of(2022, java.time.Month.JANUARY, 23, 0, 0, 0, 0))
  val orders = Order.relation(List(order1))
  val orderItem1: OrderItem.Row = (1,1,product1(0))
  val orderItem2: OrderItem.Row = (2,1,product1(0))
  val orderItem3: OrderItem.Row = (3,1,product2(0))
  val orderItems = OrderItem.relation(List(orderItem1,orderItem2, orderItem3))

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
      (1,1,1,1,"product1", BigInt(5)), 
      (1,1,1,2,"product2", BigInt(20)), 
      (2,1,1,1,"product1", BigInt(5)), 
      (2,1,1,2,"product2", BigInt(20)), 
      (3,1,2,1,"product1", BigInt(5)), 
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
    val withoutPrice = Product.tableSchema.remove(Product.price)
    val s = OrderItem.tableSchema.concat(withoutPrice)// Product.id #: Product.name #: EmptySchema)
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
    import res.schema._
    res.rows.head(Product.name) should equal("product1")
  }
  test("Extension methods to read/write property values"){
    import products.schema._
    products.rows.head(Product.name) should equal("product1")
    val product1updated = products.rows.head.updated(Product.name)("new name")
    product1updated(Product.name) should equal("new name")
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
  test("Calculate column with expr"){
    object price extends OrderItem.column[Int]
    val idGetter = orderItems.schema.propertyGetter(OrderItem.id)
    val p = orderItems.prependCalcColumn(price)({
      import OrderItem._
      import orderItems._

      rowFun(prop(id) * const(10))
    })
    val s = price #: OrderItem.tableSchema
    val res = p.projection(s)
    res.show should equal(
      """price: int, id: int, orderId: int, productId: int
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
    val p = Relation.empty[OrderItem.tableSchema.type, List](OrderItem.tableSchema)
    p.rows shouldBe empty
  }
  object sumPrice extends Product.column[BigInt]
  given ordering: Ordering[Tuple1[String]] = cats.kernel.Order[Tuple1[String]].toOrdering

  test("Expenses report with a simple groupMapReduce"){
    // SELECT product.name, sum(product.price) 
    // FROM order_item JOIN product ON order_item.product_id = product.id
    // WHERE order_item.order_id = ?
    def expensesReport[V[_], 
      P <: products.Self,
      OI <: orderItems.Self,
      ](product: P, orderItem: OI) = 
        val prod = product.crossProduct(orderItem)
        val joined = prod.filter(row => 
          prod.schema.propertyGetter(Product.id)(row) == 
            prod.schema.propertyGetter(OrderItem.productId)(row) )
        val keySchema = Product.name #: EmptySchema
        val aggregateSchema = sumPrice #: EmptySchema
        val aggregateISchema = Product.price #: EmptySchema
        // val resultSchema = keySchema.concat(aggregateSchema)
        val keyF = keySchema.projectorFrom(joined.schema)//.projection(keySchema)
        val priceAsSumPrice = aggregateISchema.projectorFrom(joined.schema)// joined.schema.projection(aggregateSchema)
        val reduced1 = joined.groupMapReduce(keyF)(priceAsSumPrice)
        reduced1 should equal(SortedMap(
          Tuple1("product1") -> Tuple1(BigInt(10)), 
          Tuple1("product2") -> Tuple1(BigInt(20))
        ))
        reduced1
        // val reduced = joined.groupMapReduceS(keySchema, aggregateSchema)(
        //   keyF,
        //   priceAsSumPrice,
        // )
        // reduced should equal(List())

    val result = expensesReport(products, orderItems)
    result should equal(SortedMap(
      Tuple1("product1") -> Tuple1(BigInt(10)), 
      Tuple1("product2") -> Tuple1(BigInt(20))
    ))
  }

  test("Expenses report with a groupMapReduce and then schema-based transformation"){
    // SELECT product.name, sum(product.price) 
    // FROM order_item JOIN product ON order_item.product_id = product.id
    // WHERE order_item.order_id = ?
    def expensesReport[V[_], 
      P <: products.Self,
      OI <: orderItems.Self,
      ](product: P, orderItem: OI) = 
        val prod = product.crossProduct(orderItem)
        val joined = prod.filter(row => 
          prod.schema.propertyGetter(Product.id)(row) == 
            prod.schema.propertyGetter(OrderItem.productId)(row) )
        val keySchema = Product.name #: EmptySchema
        val aggregateSchema = sumPrice #: EmptySchema
        val aggregateISchema = Product.price #: EmptySchema
        val resultSchema = keySchema.concat(aggregateSchema)
        val keyF = keySchema.projectorFrom(joined.schema)//.projection(keySchema)
        val priceAsSumPrice = aggregateISchema.projectorFrom(joined.schema)// joined.schema.projection(aggregateSchema)
        val reduced1 = joined.groupMapReduce(keyF)(priceAsSumPrice)
        reduced1 should equal(SortedMap(
          Tuple1("product1") -> Tuple1(BigInt(10)), 
          Tuple1("product2") -> Tuple1(BigInt(20))
        ))
        // convertSortedMapToRelation[List, keySchema.type, aggregateSchema.type](keySchema, aggregateSchema)(reduced1)
        // val vals = convertSortedMapToV[List, keySchema.type, aggregateSchema.type](keySchema, aggregateSchema)(reduced1)
        val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
        // concat
        val allVals: Iterable[resultSchema.Values] = reduced1.toIterable.map(concat(_, _))
        import cats.MonoidK.ops.toAllMonoidKOps
        val vals = allVals.foldLeft(MonoidK[List].empty[resultSchema.Values])((b, a) => b <+> Applicative[List].pure(a))
        Relation.apply(resultSchema)(vals)

    val result = expensesReport(products, orderItems)
    result.rows should equal(List(
      ("product1", BigInt(10)),
      ("product2", BigInt(20)),
    ))
  }
  test("Expenses report with a schema-based groupMapReduceS"){
    // SELECT product.name, sum(product.price) 
    // FROM order_item JOIN product ON order_item.product_id = product.id
    // WHERE order_item.order_id = ?
    def expensesReport[V[_], 
      P <: products.Self,
      OI <: orderItems.Self,
      ](product: P, orderItem: OI, orderId: Order.id.P) = 
        val prod = product.crossProduct(
          orderItem
            .filter(row => orderItem.schema.propertyGetter(OrderItem.orderId)(row) == orderId)
        )
        // TODO: DSL for predicates that use columns Product.id === OrderItem.productId
        val joined = prod
          .filter(row => 
            prod.schema.propertyGetter(Product.id)(row) == 
              prod.schema.propertyGetter(OrderItem.productId)(row) )
          
        val keySchema = Product.name #: EmptySchema
        // val aggregateSchema = sumPrice #: EmptySchema
        val aggregateISchema = Product.price #: EmptySchema
        val resultSchema = keySchema.concat(aggregateISchema)
        val keyF = keySchema.projectorFrom(joined.schema)//.projection(keySchema)
        val priceAsSumPrice = aggregateISchema.projectorFrom(joined.schema)// joined.schema.projection(aggregateSchema)
        val reduced1 = joined.groupMapReduceS(keySchema, aggregateISchema)(resultSchema)(keyF, priceAsSumPrice)
        reduced1

    val result = expensesReport(products, orderItems, 1)
    result.rows should equal(List(
      ("product1", BigInt(10)),
      ("product2", BigInt(20)),
    ))
  }
  test("Expense report with a classic DSL"){
    def expensesReport[V[_], 
      P <: products.Self,
      OI <: orderItems.Self,
      ](product: P, orderItem: OI, orderIdValue: Order.id.P) = 
        // SELECT product.name, sum(product.price) 
        // FROM order_item JOIN product ON order_item.product_id = product.id
        // WHERE order_item.order_id = ?

        // SELECT * FROM order_item WHERE order_item.order_id = ?
        
        val itemsForOrderId = {
          import OrderItem._
          import orderItem._
          filter(rowFun(prop(orderId) === const(orderIdValue)))// row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
          //tagless doesn't work yet... orderItem.filter(orderItem.expr[Boolean]([E[_]] => (e: orderItem.TaglessDsl[E]) => e.value(true)))// row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
        }
        val prod = product.crossProduct(itemsForOrderId)
        // DONE: DSL for predicates that use columns Product.id === OrderItem.productId
        val joined = {
          import prod._
          filter(rowFun(prop(Product.id) === prop(OrderItem.productId)))
        }
        val keySchema = Product.name #: EmptySchema
        // val aggregateSchema = sumPrice #: EmptySchema
        val aggregateISchema = Product.price #: EmptySchema
        val resultSchema = keySchema.concat(aggregateISchema)
        val keyF = keySchema.projectorFrom(joined.schema)//.projection(keySchema)
        val priceAsSumPrice = aggregateISchema.projectorFrom(joined.schema)// joined.schema.projection(aggregateSchema)
        val reduced1 = joined.groupMapReduceS(keySchema, aggregateISchema)(resultSchema)(keyF, priceAsSumPrice)
        reduced1

    val result = expensesReport(products, orderItems, 1)
    result.rows should equal(List(
      ("product1", BigInt(10)),
      ("product2", BigInt(20)),
    ))
  }
  test("Expense report with join"){
    def expensesReport[V[_], 
      P <: products.Self,
      OI <: orderItems.Self,
      ](product: P, orderItem: OI, orderIdValue: Order.id.P) = 
        // SELECT product.name, sum(product.price) 
        // FROM order_item JOIN product ON order_item.product_id = product.id
        // WHERE order_item.order_id = ?

        // SELECT * FROM order_item WHERE order_item.order_id = ?
        
        val itemsForOrderId = {
          import OrderItem._
          import orderItem._
          filter(rowFun(prop(orderId) === const(orderIdValue)))// row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
          //tagless doesn't work yet... orderItem.filter(orderItem.expr[Boolean]([E[_]] => (e: orderItem.TaglessDsl[E]) => e.value(true)))// row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
        }
        val joined = product.join(OrderItem.productIdFk)(itemsForOrderId)
        
        val keySchema = Product.name #: EmptySchema
        // val aggregateSchema = sumPrice #: EmptySchema
        val aggregateISchema = Product.price #: EmptySchema
        val resultSchema = keySchema.concat(aggregateISchema)
        val keyF = keySchema.projectorFrom(joined.schema)//.projection(keySchema)
        val priceAsSumPrice = aggregateISchema.projectorFrom(joined.schema)// joined.schema.projection(aggregateSchema)
        val reduced1 = joined.groupMapReduceS(keySchema, aggregateISchema)(resultSchema)(keyF, priceAsSumPrice)
        reduced1

    val result = expensesReport(products, orderItems, 1)
    result.rows should equal(List(
      ("product1", BigInt(10)),
      ("product2", BigInt(20)),
    ))
  }
