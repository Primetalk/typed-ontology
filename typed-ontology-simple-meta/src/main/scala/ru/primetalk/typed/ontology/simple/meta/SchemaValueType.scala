package ru.primetalk.typed.ontology.simple.meta

/** Тип-аннотация, позволяющий привязать к произвольному значению сведения о схеме этого значения.
  */
sealed trait WithSchema[S <: SchemaLike]

/** Приклеиваем к типу значения его схему. */
type #@[A, S <: SchemaLike] = A & WithSchema[S]

extension [A](a: A)
  def annotated[S <: SchemaLike]: A #@ S =
    a.asInstanceOf[A #@ S]

/** Type class that provides value type for the given schema. An instance of this class could be
  * used to retrieve type representation for the schema.
  */
class SchemaValueType[S <: SchemaLike, V]:
  type Schema = S
  type Value  = V
  type AValue = V #@ S

object SchemaValueType:
  def apply[S <: SchemaLike](using svt: SchemaValueType[S, ?]): svt.type =
    svt

  /** Helper type to simplify SchemaValueType search.
    */
  type Aux1[S <: SchemaLike] = SchemaValueType[S, ?]

  /** Constructs or deconstructs SchemaValueType using provided type parameters.
    */
  type Aux[S <: SchemaLike, V] = SchemaValueType[S, V]

/** Similar mechanism that isolates TupleSchema. We had to implement this auxiliary trait because
  * Scala 3.4.0 wasn't able to disambiguate plain SchemaValueType of two types that have Value <:
  * Tuple. We provide an implicit conversion though.
  */
trait TupleSchemaValueType[S <: TupleSchema]:
  type Schema = S
  type Value <: Tuple

/** Similar mechanism that isolates TupleSchema. We had to implement this auxiliary trait because
  * Scala 3.4.0 wasn't able to disambiguate plain SchemaValueType of two types that have Value <:
  * Tuple.
  */
trait RecordSchemaValueType[S <: RecordSchema]:
  type Schema = S
  type Value <: Tuple

/** Type class that returns the type of property value.
  */
trait RecordPropertyValueType[P <: RecordProperty0, V]:
  val property: P
  type Schema = property.Schema
  type Value = V

object RecordPropertyValueType:
  def apply[P <: RecordProperty0](using rpvt: RecordPropertyValueType[P, ?]): rpvt.type =
    rpvt
  type Aux1[P <: RecordProperty0] = RecordPropertyValueType[P, ?]
