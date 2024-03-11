package ru.primetalk.typed.ontology.simple.meta

class ProductSpec extends BaseSpec:
  test("all") {
    import Product.{given, *}
    import SimpleTypes.{given, *}
    val id1  = valueOf[Id]
    val s1   = summon[SchemaValueType.Aux1[ScalarSchema1[Int]]]
    val evS1 = summon[s1.Value =:= Int]

    val empty = emptySchemaSVT // summon[SchemaTupleValueType[EmptySchema]](using emptySchemaSVT)
    val ev1   = summon[empty.Value =:= EmptyTuple]
    val i1: empty.Value = EmptyTuple

    type IdRecord = Id #: EmptySchema
    val idRecord                  = id #: EmptySchema
    val idValue: ValueOf[id.type] = summon[ValueOf[id.type]]
    // val p                         = propertyValueType[id.type]

    val idSchemaSvt =
      summon[SchemaValueType.Aux1[id.Schema]] // (using scalarSchema1svt[Int, id.Schema])

    val idRecordSvt1 = tuple1Schema[Int, id.type]
    val idRecordSvt  = summon[RecordSchemaValueType.Aux1[IdRecord]] // (using tuple1Schema)
    val idRecordSvt0: RecordSchemaValueType.Aux1[IdRecord] = idRecordSvt
    val i: idRecordSvt.Value                          = Tuple1(10)

    val ev2 = summon[idRecordSvt.Value =:= Tuple1[Int]]

    val nameSvt = summon[RecordSchemaValueType.Aux1[name.type #: EmptySchema]] // (using tuple1Schema)
    val svt2    = summon[RecordSchemaValueType.Aux1[id.type #: name.type #: EmptySchema]]

    val svt = summon[RecordSchemaValueType.Aux1[id.type #: name.type #: price.type #: EmptySchema]]
    val product1: svt.Value = (1, "product1", BigInt(10))

    val svt1                 = Product.svt
    val product2: svt1.Value = (2, "product1", BigInt(10))

    val svt3                 = summon[RecordSchemaValueType.Aux1[Product.TableSchema]]
    val product3: svt3.Value = (1, "product1", BigInt(10))

    val product4: Product.Row = (3, "product1", BigInt(10))
  }
