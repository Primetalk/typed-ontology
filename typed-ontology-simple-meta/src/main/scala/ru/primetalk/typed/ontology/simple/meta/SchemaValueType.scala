package ru.primetalk.typed.ontology.simple.meta

/** Type class that provides value type for the given schema. An instance of this class could be
  * used to retrieve type representation for the schema.
  */
trait SchemaValueType:
  type Schema <: SchemaLike
  type Value

object SchemaValueType:
  def apply[S <: SchemaLike](using SchemaValueType.Aux1[S]): SchemaValueType.Aux1[S] =
    summon[SchemaValueType.Aux1[S]]

  /**
   * Helper type to simplify SchemaValueType search.
   */
  type Aux1[S <: SchemaLike] = SchemaValueType:
    type Schema = S
  
  /**
    * Constructs or deconstructs SchemaValueType using provided type parameters.
    */
  type Aux[S <: SchemaLike, V] = SchemaValueType {
    type Schema = S
    type Value = V
  }

/**
  * Similar mechanism that isolates TupleSchema.
  * We had to implement this auxiliary trait because Scala 3.4.0 wasn't able to 
  * disambiguate plain SchemaValueType of two types that have Value <: Tuple.
  * We provide an implicit conversion though.
  */
trait TupleSchemaValueType[S <: TupleSchema]:
  type Schema = S
  type Value <: Tuple

/**
  * Similar mechanism that isolates TupleSchema.
  * We had to implement this auxiliary trait because Scala 3.4.0 wasn't able to 
  * disambiguate plain SchemaValueType of two types that have Value <: Tuple.
  */
trait RecordSchemaValueType[S <: RecordSchema]:
  type Schema = S
  type Value <: Tuple

/**
  * Type class that returns the type of property value.
  */
trait RecordPropertyValueType[P <: RecordProperty0]:
  type Value
