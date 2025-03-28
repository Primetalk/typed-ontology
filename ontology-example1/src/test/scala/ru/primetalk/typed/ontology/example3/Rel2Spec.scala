package ru.primetalk.typed.ontology.example3

import ru.primetalk.typed.ontology.example1.{Product, Order, OrderItem}
import java.time.LocalDateTime
// import cats.instances._
import ru.primetalk.typed.ontology.simple.meta.#:
import ru.primetalk.typed.ontology.simple.meta.EmptySchema
import ru.primetalk.typed.ontology.simple.relalg.Relation
// import ru.primetalk.typed.ontology.simple.relalg.convertSortedMapToRelation
// import ru.primetalk.typed.ontology.simple.relalg.convertSortedMapToV
import ru.primetalk.typed.ontology.simple.meta.SimplePropertyId
import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.relalg.relation
import ru.primetalk.typed.ontology.simple.meta.SimpleTypes
import SimpleTypes.{given, *}
import scala.collection.immutable.SortedMap
import cats.MonoidK
import cats.Applicative
import ru.primetalk.typed.ontology.simple.meta.{Projector, PropertyProjector}
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType
import ru.primetalk.typed.ontology.simple.meta.RecordPropertyValueType

trait TestDataRel2 extends BaseSpec:
  val product1: Product.Row = (1, "product1", BigInt(5))
  val (product1id, _, _)    = product1
  val product2: Product.Row = (2, "product2", BigInt(20))
  val (product2id, _, _)    = product2
  val products              = Product.relation(List(product1, product2))
  val order1: Order.Row     = (1, LocalDateTime.of(2022, java.time.Month.JANUARY, 23, 0, 0, 0, 0))
  val orders                = Order.relation(List(order1))
  val orderItem1: OrderItem.Row = (1, 1, product1id)
  val orderItem2: OrderItem.Row = (2, 1, product1id)
  val orderItem3: OrderItem.Row = (3, 1, product2id)
  val orderItems                = OrderItem.relation(List(orderItem1, orderItem2, orderItem3))

class Rel2Spec extends TestDataRel2:

  test("Property to string") {
    println(Product.id.toString)
    assertResult("id: int")(Product.id.toString)
  }
  test("availability of Projector") {
    val p0 = summon[Projector[Product.Id #: EmptySchema, ?, EmptySchema, ?]]
    val p1 = summon[Projector[Product.Id #: EmptySchema, ?, Product.Id #: EmptySchema, ?]](
      using someSchemaPlusPropertyProjector
    )
    val pKey                = summon[Projector[Product.TableSchema, ?, Product.PrimaryKeySchema, ?]]
    val svt                 = summon[SchemaValueType.Aux1[Product.TableSchema]]
    val product1: svt.Value = (1, "name", BigInt(1))
    val product2: Product.Row = (1, "name", BigInt(1))
    pKey.apply(product1)
  }
  test("projection") {
    val ids = products.projection(Product.primaryKeySchema)
    ids.rows should equal(List(Tuple(1), Tuple(2)))
    ids.schema.toString should equal(Product.primaryKeySchema.toString)
  }
  test("schema concat") {
    val schema3 = products.schema.concat(orderItems.schema)
    schema3.toString should equal(products.schema.toString + ", " + orderItems.schema.toString)
  }
//  test("cross product from") {
//    val poi = products.crossProductFrom(orderItems)
//    poi.rows should equal(
//      List(
//        (1, 1, 1, 1, "product1", BigInt(5)),
//        (1, 1, 1, 2, "product2", BigInt(20)),
//        (2, 1, 1, 1, "product1", BigInt(5)),
//        (2, 1, 1, 2, "product2", BigInt(20)),
//        (3, 1, 2, 1, "product1", BigInt(5)),
//        (3, 1, 2, 2, "product2", BigInt(20))
//      )
//    )
//  }
  test("ValueOf") {
    summon[ValueOf[Product.name.type]].value should equal(Product.name)
  }
  test("constSchema") {
    type S1 = Product.name.type #: Product.price.type #: EmptySchema
    val s2     = Product.fields(Product.name, Product.price)
    val s1: S1 = RecordSchema.constSchema[S1]
    s1 should equal(s2)
  }
  test("Check Product.id") {
    val id = Product.id
    id should equal(id)
    s"$id" should equal("id: int")
  }

  test("Check schema") {
    val s1 = Product.tableSchema
    val s2 = Product.id #: Product.name #: Product.price #: EmptySchema
    s1 should equal(s2)
  }
  test("Remove first property from schema") {
    val s1 = Product.tableSchema.remove(Product.id)
    val s2 = Product.name #: Product.price #: EmptySchema
    s1 should equal(s2)
  }
  test("Remove middle property from schema") {
    val s1 = Product.tableSchema.remove(Product.name)
    val s2 = Product.id #: Product.price #: EmptySchema
    s1 should equal(s2)
  }
  test("Remove last property from schema") {
    val s1 = Product.tableSchema.remove(Product.price)
    val s2 = Product.id #: Product.name #: EmptySchema
    s1 should equal(s2)
  }
  test("cross product") {
    val poi = orderItems.crossProduct(products)
    poi.rows should equal(
      List(
        (1, 1, 1, 1, "product1", 5),
        (1, 1, 1, 2, "product2", 20),
        (2, 1, 1, 1, "product1", 5),
        (2, 1, 1, 2, "product2", 20),
        (3, 1, 2, 1, "product1", 5),
        (3, 1, 2, 2, "product2", 20)
      )
    )
    val withoutPrice = Product.tableSchema.remove(Product.price)
    val s1 =
      OrderItem.tableSchema.concat(withoutPrice) // Product.id #: Product.name #: EmptySchema)
    val s = OrderItem.tableSchema.concat(
      Product.idNameSchema
    ) // Product.id #: Product.name #: EmptySchema)
    val res = poi.projection(s)
    res.rows should equal(
      List(
        (1, 1, 1, 1, "product1"),
        (1, 1, 1, 2, "product2"),
        (2, 1, 1, 1, "product1"),
        (2, 1, 1, 2, "product2"),
        (3, 1, 2, 1, "product1"),
        (3, 1, 2, 2, "product2")
      )
    )
    // import res.schema._
    // res.rows.head(Product.name) should equal("product1")
  }
  test("Extension methods to read/write property values") {
    import products.schema._
    val svt       = summon[SchemaValueType.Aux1[Product.TableSchema]]
    val priceRpvt = summon[RecordPropertyValueType[Product.Price, BigInt]]
    val priceSvt  = summon[SchemaValueType.Aux1[priceRpvt.Schema]]
    val t1priceSvt= summon[SchemaValueType.Aux1[Product.PriceSchema]]
    val t2namepriceSvt= summon[SchemaValueType.Aux1[Product.NamePriceSchema]]
    val prjPrice =
      summon[PropertyProjector[Product.PriceSchema, t1priceSvt.Value, Product.Price, BigInt]]
    val prjPrice2I = propertyProjectorTail[
      BigInt,
      Product.Price,
      String,
      Product.Name,

      Product.PriceSchema,
      // Product.NamePriceSchema,
      Tuple1[BigInt]
      ]//(using priceRpvt, prjPrice, t2namepriceSvt)
    val prjPrice2 =
      summon[PropertyProjector[Product.NamePriceSchema, t2namepriceSvt.Value, Product.price.type, BigInt]](
        using
        prjPrice2I
      )
    // val prjPrice2o =
    //   summon[Projector[Product.NamePriceSchema, t2namepriceSvt.Value, Product.price.Schema, BigInt]]
    val prjPrice3 =
      summon[PropertyProjector[Product.TableSchema, svt.Value, Product.price.type, BigInt]](
        using
        propertyProjectorTail//(using priceRpvt, prjPrice2, svt)
      )
    val prj = summon[PropertyProjector[Product.TableSchema, svt.Value, Product.name.type, String]]//(using propertyProjectorOther)
    val v: svt.Value = products.rows.head
    new ValueOps(v)(using svt)/Product.name1 should equal("product1")
    // new ValueOps(v)(using svt)./(Product.name) should equal("product1")
    // val product1updated = products.rows.head.updated(Product.name)("new name")
    // product1updated(Product.name) should equal("new name")
  }
  test("Calculate column") {
    object price extends OrderItem.column[Long]
    // val idGetter = orderItems.schema.propertyGetter(OrderItem.id)
    val p = orderItems.prependCalcColumn(price)(row => row._1 * 10L)
    p.show should equal(
      """price: long, id: int, orderId: int, productId: int
        |-----
        |(10,1,1,1)
        |(20,2,1,1)
        |(30,3,1,2)""".stripMargin
    )
  }
  test("Calculate column with expr") {
    object price extends OrderItem.column[Int]
    import orderItems._
    val p = orderItems.prependCalcColumn(price)(
      rowFun:
        prop(OrderItem.id) * const(10)
    )
    // TODO: test with a newer version of Scala
    // For some reason the following doesn't work:
    import orderItems._
    val expr = prop(OrderItem.id) * const(10)
    val p2 = orderItems.prependCalcColumnF(price)(
      prop(OrderItem.id)// * const(10)
    )
    p.show should equal(
      """price: int, id: int, orderId: int, productId: int
        |-----
        |(10,1,1,1)
        |(20,2,1,1)
        |(30,3,1,2)""".stripMargin
    )
  }
  test("Rename column") {
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
  test("Filter") {
    // val idGetter = orderItems.schema.propertyGetter(OrderItem.id)
    object id2 extends OrderItem.column[Int]
    val p = orderItems.filter(_._1 == 1)
    p.rows should equal(List(orderItem1))
  }
  test("Union") {
    val p = orderItems ++ orderItems
    p.rows.size should equal(orderItems.rows.size * 2)
  }

  test("empty") {
    val p = Relation.empty[OrderItem.TableSchema, OrderItem.Row, List](OrderItem.tableSchema)
    p.rows shouldBe empty
  }
  object sumPrice extends Product.column[BigInt]
  given ordering: Ordering[Tuple1[String]] = cats.kernel.Order[Tuple1[String]].toOrdering

//   test("Expenses report with a simple groupMapReduce") {
//     // SELECT product.name, sum(product.price)
//     // FROM order_item JOIN product ON order_item.product_id = product.id
//     // WHERE order_item.order_id = ?
//     transparent inline def expensesReport[V[_], P <: products.Self, OI <: orderItems.Self](
//         product: P,
//         orderItem: OI
//     ) =
//       val prod = product.crossProduct(orderItem)
//       val joined = prod.filter(row =>
//         prod.schema.propertyGetter(Product.id)(row) ==
//           prod.schema.propertyGetter(OrderItem.productId)(row)
//       )
//       val keySchema        = Product.name #: EmptySchema
//       val aggregateSchema  = sumPrice #: EmptySchema
//       val aggregateISchema = Product.price #: EmptySchema
//       // val resultSchema = keySchema.concat(aggregateSchema)
//       val keyF = keySchema.projectorFrom[joined.Schema](joined.schema) // .projection(keySchema)
//       val priceAsSumPrice =
//         aggregateISchema.projectorFrom(joined.schema) // joined.schema.projection(aggregateSchema)
//       val reduced1 = joined.groupMapReduce(keyF)(priceAsSumPrice)
//       reduced1 should equal(
//         SortedMap(
//           Tuple1("product1") -> Tuple1(BigInt(10)),
//           Tuple1("product2") -> Tuple1(BigInt(20))
//         )
//       )
//       reduced1
//       // val reduced = joined.groupMapReduceS(keySchema, aggregateSchema)(
//       //   keyF,
//       //   priceAsSumPrice,
//       // )
//       // reduced should equal(List())

//     val result = expensesReport(products, orderItems)
//     result should equal(
//       SortedMap(
//         Tuple1("product1") -> Tuple1(BigInt(10)),
//         Tuple1("product2") -> Tuple1(BigInt(20))
//       )
//     )
//   }

//   test("Expenses report with a groupMapReduce and then schema-based transformation") {
//     // SELECT product.name, sum(product.price)
//     // FROM order_item JOIN product ON order_item.product_id = product.id
//     // WHERE order_item.order_id = ?
//     def expensesReport[V[_], P <: products.Self, OI <: orderItems.Self](product: P, orderItem: OI) =
//       val prod = product.crossProduct(orderItem)
//       val joined = prod.filter(row =>
//         prod.schema.propertyGetter(Product.id)(row) ==
//           prod.schema.propertyGetter(OrderItem.productId)(row)
//       )
//       val keySchema        = Product.name #: EmptySchema
//       val aggregateSchema  = sumPrice #: EmptySchema
//       val aggregateISchema = Product.price #: EmptySchema
//       val resultSchema     = keySchema.concat(aggregateSchema)
//       val keyF             = keySchema.projectorFrom(joined.schema) // .projection(keySchema)
//       val priceAsSumPrice =
//         aggregateISchema.projectorFrom(joined.schema) // joined.schema.projection(aggregateSchema)
//       val reduced1 = joined.groupMapReduce(keyF)(priceAsSumPrice)
//       reduced1 should equal(
//         SortedMap(
//           Tuple1("product1") -> Tuple1(BigInt(10)),
//           Tuple1("product2") -> Tuple1(BigInt(20))
//         )
//       )
//       // convertSortedMapToRelation[List, keySchema.type, aggregateSchema.type](keySchema, aggregateSchema)(reduced1)
//       // val vals = convertSortedMapToV[List, keySchema.type, aggregateSchema.type](keySchema, aggregateSchema)(reduced1)
//       val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
//       // concat
//       val allVals: Seq[resultSchema.Values] = reduced1.toSeq.map(concat(_, _))
//       import cats.syntax.semigroupk.toSemigroupKOps
//       val vals = allVals.foldLeft(MonoidK[List].empty[resultSchema.Values])((b, a) =>
//         b <+> Applicative[List].pure(a)
//       )
//       Relation.apply(resultSchema)(vals)

//     val result = expensesReport(products, orderItems)
//     result.rows should equal(
//       List(
//         ("product1", BigInt(10)),
//         ("product2", BigInt(20))
//       )
//     )
//   }
//   test("Expenses report with a schema-based groupMapReduceS") {
//     // SELECT product.name, sum(product.price)
//     // FROM order_item JOIN product ON order_item.product_id = product.id
//     // WHERE order_item.order_id = ?
//     def expensesReport[V[_], P <: products.Self, OI <: orderItems.Self](
//         product: P,
//         orderItem: OI,
//         orderId: Order.id.P
//     ) =
//       val prod = product.crossProduct(
//         orderItem
//           .filter(row => orderItem.schema.propertyGetter(OrderItem.orderId)(row) == orderId)
//       )
//       // TODO: DSL for predicates that use columns Product.id === OrderItem.productId
//       val joined = prod
//         .filter(row =>
//           prod.schema.propertyGetter(Product.id)(row) ==
//             prod.schema.propertyGetter(OrderItem.productId)(row)
//         )

//       val keySchema = Product.name #: EmptySchema
//       // val aggregateSchema = sumPrice #: EmptySchema
//       val aggregateISchema = Product.price #: EmptySchema
//       val resultSchema     = keySchema.concat(aggregateISchema)
//       val keyF             = keySchema.projectorFrom(joined.schema) // .projection(keySchema)
//       val priceAsSumPrice =
//         aggregateISchema.projectorFrom(joined.schema) // joined.schema.projection(aggregateSchema)
//       val reduced1 = joined.groupMapReduceS(keySchema, aggregateISchema)(keyF, priceAsSumPrice)
//       reduced1

//     val result = expensesReport(products, orderItems, 1)
//     result.rows should equal(
//       List(
//         ("product1", BigInt(10)),
//         ("product2", BigInt(20))
//       )
//     )
//   }
//   test("Expense report with a classic DSL") {
//     def expensesReport[V[_], P <: products.Self, OI <: orderItems.Self](
//         product: P,
//         orderItem: OI,
//         orderIdValue: Order.id.P
//     ) =
//       // SELECT product.name, sum(product.price)
//       // FROM order_item JOIN product ON order_item.product_id = product.id
//       // WHERE order_item.order_id = ?

//       // SELECT * FROM order_item WHERE order_item.order_id = ?

//       val itemsForOrderId = {
//         import OrderItem._
//         import orderItem._
//         filter(
//           rowFun(prop(orderId) === const(orderIdValue))
//         ) // row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
//         // tagless doesn't work yet... orderItem.filter(orderItem.expr[Boolean]([E[_]] => (e: orderItem.TaglessDsl[E]) => e.value(true)))// row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
//       }
//       val prod = product.crossProduct(itemsForOrderId)
//       // DONE: DSL for predicates that use columns Product.id === OrderItem.productId
//       val joined = {
//         import prod._
//         filter(rowFun(prop(Product.id) === prop(OrderItem.productId)))
//       }
//       val keySchema = Product.name #: EmptySchema
//       // val aggregateSchema = sumPrice #: EmptySchema
//       val aggregateISchema = Product.price #: EmptySchema
//       val resultSchema     = keySchema.concat(aggregateISchema)
//       val keyF             = keySchema.projectorFrom(joined.schema) // .projection(keySchema)
//       val priceAsSumPrice =
//         aggregateISchema.projectorFrom(joined.schema) // joined.schema.projection(aggregateSchema)
//       val reduced1 = joined.groupMapReduceS(keySchema, aggregateISchema)(keyF, priceAsSumPrice)
//       reduced1

//     val result = expensesReport(products, orderItems, 1)
//     result.rows should equal(
//       List(
//         ("product1", BigInt(10)),
//         ("product2", BigInt(20))
//       )
//     )
//   }
//   test("Expense report with join") {
//     def expensesReport[V[_], P <: products.Self, OI <: orderItems.Self](
//         product: P,
//         orderItem: OI,
//         orderIdValue: Order.id.P
//     ) =
//       // SELECT product.name, sum(product.price)
//       // FROM order_item JOIN product ON order_item.product_id = product.id
//       // WHERE order_item.order_id = ?

//       // SELECT * FROM order_item WHERE order_item.order_id = ?

//       val itemsForOrderId = {
//         import OrderItem._
//         import orderItem._
//         filter(
//           rowFun(prop(orderId) === const(orderIdValue))
//         ) // row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
//         // tagless doesn't work yet... orderItem.filter(orderItem.expr[Boolean]([E[_]] => (e: orderItem.TaglessDsl[E]) => e.value(true)))// row => orderItem.schema.propertyGetter(orderId)(row) == orderIdValue)
//       }
//       val joined = product.join(OrderItem.productIdFk)(itemsForOrderId)

//       val keySchema = Product.name #: EmptySchema
//       // val aggregateSchema = sumPrice #: EmptySchema
//       val aggregateISchema = Product.price #: EmptySchema
//       val resultSchema     = keySchema.concat(aggregateISchema)
//       val keyF = keySchema.projectorFrom[joined.Schema](joined.schema) // .projection(keySchema)
//       val priceAsSumPrice =
//         aggregateISchema.projectorFrom(joined.schema) // joined.schema.projection(aggregateSchema)
//       val reduced1 = joined.groupMapReduceS(keySchema, aggregateISchema)(keyF, priceAsSumPrice)
//       reduced1

//     val result = expensesReport(products, orderItems, 1)
//     result.rows should equal(
//       List(
//         ("product1", BigInt(10)),
//         ("product2", BigInt(20))
//       )
//     )
//   }
