package ru.primetalk.typed.ontology.typeclass.fschema

import ru.primetalk.typed.ontology.typeclass.schema.SchemaValueType

/**
  * Annotation of value with schema. Could be used with records (tuples) or with plain values.
  */
opaque type ValueWithSchemaF[F[+_], Schema, +Value] >: F[Value] = F[Value]

object ValueWithSchemaF:

  private class IdentityConversion[A] extends Conversion[A, A]:
    def apply(x: A): A = x

  // inline given valueToValueWithSchema[Schema, Value](using ev: SchemaValueType[Schema, Value]): Conversion[Value, ValueWithSchemaF[Schema, Value]] = 
  //   new IdentityConversion[ValueWithSchemaF[Schema, Value]]

  // extension [Schema, Value](vws: ValueWithSchemaF[Schema, Value])
  //   def value: Value = vws

end ValueWithSchemaF
