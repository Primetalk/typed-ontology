package ru.primetalk.typed.ontology.typeclass.schema

/**
  * Annotation of value with schema. Could be used with records (tuples) or with plain values.
  */
opaque type ValueWithSchema[Schema, +Value] >: Value = Value

object ValueWithSchema:

  private class IdentityConversion[A] extends Conversion[A, A]:
    def apply(x: A): A = x

  inline given valueToValueWithSchema[Schema, Value](using ev: SchemaValueType[Schema, Value]): Conversion[Value, ValueWithSchema[Schema, Value]] = 
    new IdentityConversion[ValueWithSchema[Schema, Value]]

  extension [Schema, Value](vws: ValueWithSchema[Schema, Value])
    def value: Value = vws

end ValueWithSchema
