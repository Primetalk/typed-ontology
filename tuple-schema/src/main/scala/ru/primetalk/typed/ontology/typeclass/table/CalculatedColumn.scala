package ru.primetalk.typed.ontology.typeclass.table

import ru.primetalk.typed.ontology.typeclass.schema.{Getter, SchemaValueType, ValueWithSchema}

case class FromDSL[From](): 
  class CalculatedColumn[Name <: String & Singleton, Value](val f: From => Value)

  object CalculatedColumn:

    given [Name <: String & Singleton, Value, C <: CalculatedColumn[Name, Value]](using v: ValueOf[C]): Getter[C, Value, From] = 
      new:
        private val c: C = summon[ValueOf[C]].value

        def apply(rtc: From): ValueWithSchema[C, Value] = 
          c.f(rtc)

    given [Name <: String & Singleton, Value, C <: CalculatedColumn[Name, Value]](using v: ValueOf[C]): SchemaValueType[C, Value] =
      new SchemaValueType

  end CalculatedColumn
