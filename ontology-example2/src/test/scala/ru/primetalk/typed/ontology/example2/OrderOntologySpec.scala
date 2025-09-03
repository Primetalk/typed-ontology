package ru.primetalk.typed.ontology.example2

import scala.language.experimental.namedTuples

import ru.primetalk.typed.ontology.typeclass.schema.{
  Getter,
  Projector,
  RecordTupleValue,
  SchemaValueType
}
import ru.primetalk.typed.ontology.typeclass.schema.RecordTupleValue.{*, given}
import ru.primetalk.typed.ontology.typeclass.table.TableColumn
import ru.primetalk.typed.ontology.typeclass.table.TableColumn.{*, given}
import ru.primetalk.typed.ontology.typeclass.table.FromDSL
import ru.primetalk.typed.ontology.typeclass.schema.RuntimeNames
import scala.runtime.Tuples

class OrderOntologySpec extends BaseSpec:

  test("get value") {
    val row: Product.Row = (1, "product name", BigInt(1))
    val ev1 = summon[(Int, String, BigInt) <:< Product.Row]
    val getter = summon[Getter[
      Product.id,
      Int,
      RecordTupleValue[Product.TableSchema, Int *: String *: BigInt *: EmptyTuple]
    ]]
    val svt1 = summon[SchemaValueType[Product.id, Int]]
    val svt  = SchemaValueType.Aux[Product.id]
    assert(row.get(Product.id) == 1)
    assert(row.project(Product.priceSchema) == BigInt(1) *: EmptyTuple)
    assert(row.project(Product.idNameSchema) == (1, "product name"))
  }

  test("named tuples") {
    val row: Product.Row = (1, "product name", BigInt(1))
    val a                = row.toNamedTuple
    assert(a.id == 1)
    assert(a.name == "product name")

    val aPlain: (Int, String, BigInt) = a.toTuple
    val row2: Product.Row = RecordTupleValue[Product.TableSchema](aPlain)
    assert(row == row2)

    val row3: Product.Row = Product.fullSchema.fromNamedTuple(a)
    assert(row == row3)

    val row4: Product.Row = RecordTupleValue.fromNamedTuple[Product.TableSchema](a)
    assert(row == row4)
  }

  test("nested get value") {
    val p1: Person.Row  = ("name", ("street", 1))
    val a1              = p1.get(Person.address)
    val a2: Address.Row = p1.get(Person.address)
    assert(p1.get(Person.address) == ("street", 1))
    assert(p1.get(Person.address).get(Address.street) == "street")
  }

  test("calculated column") {
    val p1: Person.Row = ("name", ("street", 1))
    val f              = FromDSL[Person.Row]()
    object NameStreet
        extends f.CalculatedColumn["NameStreet", String](row =>
          row.get(Person.name) + row.get(Person.address).get(Address.street)
        )

    type NameStreet    = NameStreet.type
    type PersonSchema2 = NameStreet *: Person.TableSchema
    val PersonSchema2 = NameStreet *: Person.tableSchema
    val p2            = p1.project(PersonSchema2)
    assert(p2 == ("namestreet", "name", ("street", 1)))
  }

  test("runtime names") {
    val rn = summon[RuntimeNames[Person.TableSchema]]
    assert(rn.names == List("name", "address"))
  }

  test("map from/to case class"){
    val row: Product.Row = (1, "product name", BigInt(1))
    case class MyProduct1(id: Int, name: String, price: BigInt)
    val p1 = MyProduct1.apply.tupled(row.toTuple)
    val p2: MyProduct1 = row.as
    assert(p1 == p2)
    val row2 = p1.asRecord[Product.TableSchema]
    assert(p1.id == 1)
    assert(row2 == row)
    val row3 = MyProduct1(1, "product name", 1).asRecord[Product.TableSchema]
    assert(row3 == row)
  }
