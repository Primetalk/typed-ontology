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

class OrderOntologySpec extends BaseSpec:

  test("get value") {

    val row: Product.Row = (1, "product name", BigInt(1))
    val a                = row.toNamedTuple
    assert(a.id == 1)
    assert(a.name == "product name")
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
