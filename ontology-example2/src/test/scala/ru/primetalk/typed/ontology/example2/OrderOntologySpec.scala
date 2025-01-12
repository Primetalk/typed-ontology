package ru.primetalk.typed.ontology.example2

import ru.primetalk.typed.ontology.typeclass.schema.{Getter, RecordTupleValue, SchemaValueType}
import ru.primetalk.typed.ontology.typeclass.schema.RecordTupleValue.{*, given}
import ru.primetalk.typed.ontology.typeclass.table.TableColumn
import ru.primetalk.typed.ontology.typeclass.table.TableColumn.{*, given}

class OrderOntologySpec extends BaseSpec:

  test("get value"){

    val row: Product.Row = (1, "product name", BigInt(1))
    val getter = summon[Getter[Product.id, Int, Product.id *: Product.name *: Product.price *: EmptyTuple, Int *: String *: BigInt *: EmptyTuple]]
    val svt1 = summon[SchemaValueType[Product.id, Int]]
//    val svt = SchemaValueType.Aux[Product.id]
//    val ev = summon[svt.Value =:= Int]
//    assert(row.get[Product.id, Int](using svt)(using getter) == 1)
  }
