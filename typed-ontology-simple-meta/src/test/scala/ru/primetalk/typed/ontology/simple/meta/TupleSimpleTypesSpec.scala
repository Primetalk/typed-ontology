package ru.primetalk.typed.ontology.simple.meta

class TupleSimpleTypesSpec extends BaseSpec:

  object SimpleTypes1 extends TupleSimpleTypes 
    with ScalarSimpleTypes 
    with ConversionSimpleTypes 
    with PropertySimpleTypes

  test("all") {
    import Product.{given, *}
    import SimpleTypes1.{given, *}

    import ScalarSchema1.given

    val emptyTupleSchema    = EmptyTupleSchema
    val emptyTupleSchemaSvt = summon[SchemaValueType.Aux1[emptyTupleSchema.type]]
    val evid1               = summon[emptyTupleSchemaSvt.Value =:= EmptyTuple]
    val intSchema           = summon[ScalarSchema1[Int]]
    val intSchemaSvt =
      summon[SchemaValueType.Aux1[intSchema.type]] // (using scalarSchema1svt[Int, intSchema.type])
    val evScalarInt = summon[intSchemaSvt.Value =:= (Int)]

    val stringSchema = summon[ScalarSchema1[String]]
    val stringTuple =
      NonEmptyTupleSchema[stringSchema.type, emptyTupleSchema.type](stringSchema, emptyTupleSchema)
    val stringTupleSvt = summon[SchemaValueType.Aux1[stringTuple.type]] // (using nonEmptyTupleSchema)
    val evScalar1      = summon[stringTupleSvt.Value =:= Tuple1[String]]

    val scalar2 = NonEmptyTupleSchema(intSchema, NonEmptyTupleSchema(stringSchema, EmptyTupleSchema))
    val scalar2Svt = summon[SchemaValueType.Aux1[scalar2.type]] // (using nonEmptyTupleSchema)
    val evScalar2  = summon[scalar2Svt.Value =:= (Int, String)]

    println(Product.id1)
    implicit val name1: Product.name.type = Product.name
    
    val rpvt1 = propertyValueType[String, Product.Name]
    // assert(rpvt1.property == Product.name)
      
    val rpvt = summon[RecordPropertyValueType[Product.Name, String]]
    summon[rpvt.Value =:= String]
  }

  test("property projectors"){
    import SimpleTypes.{*, given}
    val rpvt = summon[RecordPropertyValueType[Product.Price, BigInt]]
    // assert(rpvt.property == Product.price)
    val svtp = summon[SchemaValueType[rpvt.Schema, BigInt]]
    summon[svtp.Value =:= BigInt]
    val svtps = summon[SchemaValueType[Product.PriceSchema, Tuple1[BigInt]]]
    summon[svtps.Value =:= Tuple1[BigInt]]

    require(Product.price != null)
    val prj = propertyProjectorHead(using rpvt, svtp, svtps)
    val priceProjector = summon[PropertyProjector[Product.PriceSchema, Tuple1[BigInt], Product.Price, BigInt]](
      using prj
    )
    assert(priceProjector(Tuple1(BigInt(1))) == BigInt(1))

  }
