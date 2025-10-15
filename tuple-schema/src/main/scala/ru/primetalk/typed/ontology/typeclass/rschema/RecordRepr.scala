package ru.primetalk.typed.ontology.typeclass.rschema

import ru.primetalk.typed.ontology.typeclass.schema.{SchemaValueType, ValueWithSchema}

trait RecordRepr[Base]:
  /**
    * Type after prepending column of schema HS with value type HV to
    * an existing schema S with value representation A
    */
  type Prepend[HS, HV, S <: Tuple, A <: Base]
  inline def prepend[HS, HV, S <:Tuple, A <: Base](h: HV, value: ValueWithSchema[S, A])(using SchemaValueType[S, A]): Prepend[HS, HV, S, A] 

  /**
    * Type result of getting value from schema. Usually it's HV | Nothing
    */
  type Get[HS, HV, S <: Tuple, A <: Base] <: HV | Nothing
  inline def get[HS, HV, S <: Tuple, A <: Base](value: ValueWithSchema[S, A]): Get[HS, HV, S, A]

  inline def getOpt[HS, HV, S <: Tuple, A <: Base](value: ValueWithSchema[S, A]): Option[HV]


object TupleReprs:
  type Get0[HS, HV, S <: Tuple] <: HV | Nothing = S match 
    case HS *: tail => HV
    case _  *: tail => Get0[HS, HV, tail]
    case EmptyTuple => Nothing

  type Get0Opt[HS, HV, S <: Tuple] <: Option[HV] = S match 
    case HS *: tail => Some[HV]
    case _  *: tail => Get0Opt[HS, HV, tail]
    case EmptyTuple => None.type

  given RecordRepr[Tuple] = new RecordRepr[Tuple]:
    type Prepend[HS, HV, S <: Tuple, A <: Tuple] = 
      HV *: A
    inline def prepend[HS, HV, S <: Tuple, A <: Tuple](h: HV, value: ValueWithSchema[S, A])(using SchemaValueType[S, A]): Prepend[HS, HV, S, A] =
      h *: value.value

    type Get[HS, HV, S <: Tuple, A <: Tuple] = Get0[HS, HV, S]
    inline def get[HS, HV, S <: Tuple, A <: Tuple](value: ValueWithSchema[S, A]): Get[HS, HV, S, A] =
      inline scala.compiletime.erasedValue[S] match
        case _: (HS *: tail) => (value.value.head).asInstanceOf[HV]
        case _: (_  *: tail) => get(value.value.tail)
        case _: EmptyTuple   => ???
      

    inline def getOpt[HS, HV, S <: Tuple, A <: Tuple](value: ValueWithSchema[S, A]): Option[HV] =
      inline scala.compiletime.erasedValue[S] match
        case _: (HS *: tail) => Some((value.value.head).asInstanceOf[HV])
        case _: (_  *: tail) => getOpt(value.value.tail)
        case _: EmptyTuple   => None

  // given [Column, ColumnValue, S <: Tuple, Value](using repr: RecordRepr[Value], svt: SchemaValueType[S, Value]): ru.primetalk.typed.ontology.typeclass.schema.Getter[Column, ColumnValue, Value] = 
  //   new:
  //     def apply(rtc: Value): ValueWithSchema[Column, ColumnValue] = 
  //       repr.get[Column, ColumnValue, S, Value](rtc)
