package ru.primetalk.typed.ontology.simple.meta

/** Type class that provides value type for the given schema.
 * An instance of this class could be used to retrieve type representation for the schema.
 */
trait SchemaValueType[S <: SchemaLike]:
  type Value

trait SchemaTupleValueType[S <: TupleSchema] extends SchemaValueType[S]:
  type Value <: Tuple
