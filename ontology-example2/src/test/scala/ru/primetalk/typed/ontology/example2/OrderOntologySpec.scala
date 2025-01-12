package ru.primetalk.typed.ontology.example2

import ru.primetalk.typed.ontology.typeclass.schema.{Getter, RecordTupleValue, SchemaValueType}
import ru.primetalk.typed.ontology.typeclass.schema.RecordTupleValue.{*, given}
import ru.primetalk.typed.ontology.typeclass.table.TableColumn
import ru.primetalk.typed.ontology.typeclass.table.TableColumn.{*, given}

class OrderOntologySpec extends BaseSpec:

  test("get value"){

    val row: Product.Row = (1, "product name", BigInt(1))
    val ev1 = summon[(Int, String, BigInt) <:< Product.Row]
    val getter = summon[Getter[Product.id, Int, RecordTupleValue[Product.TableSchema, Int *: String *: BigInt *: EmptyTuple]]]
    val svt1 = summon[SchemaValueType[Product.id, Int]]
    val svt = SchemaValueType.Aux[Product.id]
    assert(row.get(Product.id) == 1)
  }
