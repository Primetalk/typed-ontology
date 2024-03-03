package ru.primetalk.typed.ontology.simple.meta

class TupleSimpleTypesSpec:

  import Product.{given, *}
  object SimpleTypes1 extends TupleSimpleTypes with ScalarSimpleTypes with ConversionSimpleTypes
  import SimpleTypes1.{given, *}

  import ScalarSchema1.given

  val emptyTupleSchema = EmptyTupleSchema
  val emptyTupleSchemaSvt = summon[SchemaValueType.Aux1[emptyTupleSchema.type]]
  val evid1 = summon[emptyTupleSchemaSvt.Value =:= EmptyTuple]
  val intSchema = summon[ScalarSchema1[Int]]
  val intSchemaSvt = summon[SchemaValueType.Aux1[intSchema.type]]//(using scalarSchema1svt[Int, intSchema.type])
  val evScalarInt = summon[intSchemaSvt.Value =:= (Int)]

  val stringSchema = summon[ScalarSchema1[String]]
  val stringTuple = NonEmptyTupleSchema[stringSchema.type,emptyTupleSchema.type](stringSchema, emptyTupleSchema)
  val stringTupleSvt = summon[SchemaValueType.Aux1[stringTuple.type]]//(using nonEmptyTupleSchema)
  val evScalar1 = summon[stringTupleSvt.Value =:= Tuple1[String]]

  val scalar2 = NonEmptyTupleSchema(intSchema, NonEmptyTupleSchema(stringSchema, EmptyTupleSchema))
  val scalar2Svt = summon[SchemaValueType.Aux1[scalar2.type]]//(using nonEmptyTupleSchema)
  val evScalar2 = summon[scalar2Svt.Value =:= (Int, String)]

