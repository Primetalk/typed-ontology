package ru.primetalk.typed.ontology.simple.relalg

import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.RecordProperty0

// // doesn't work at the moment
// trait TaglessPredicateDsl:
//   type Schema <: RecordSchema
//   val schema: Schema
//   type Row = schema.Values

//   trait TaglessDsl[E[_]]:
//     inline def getter[T](inline name: String, inline f: Row => T): E[T]
//     inline def property[P <: RecordProperty0](p: P): E[RecordProperty0.PropertyValueType[p.type]]
//     inline def value[T](inline v: T): E[T]
//     inline def equals[T](inline e1: E[T], inline e2: E[T]): E[Boolean]

//   type RowFun = [T] =>> Row => T

//   object taglessEval extends TaglessDsl[RowFun]: // [T] =>> Row => T]:
//     inline def getter[T](inline name: String, inline f: Row => T): Row => T = f
//     inline def property[P <: RecordProperty0](
//         p: P
//     ): Row => RecordProperty0.PropertyValueType[p.type] =
//       schema.propertyGetter(p)
//     inline def value[T](inline v: T): Row => T = _ => v
//     inline def equals[T](inline e1: Row => T, inline e2: Row => T): Row => Boolean =
//       r => e1(r) == e2(r)

//   object taglessShow extends TaglessDsl[[T] =>> String]:
//     inline def getter[T](inline name: String, inline f: Row => T): String = name
//     inline def property[P <: RecordProperty0](p: P): String =
//       p.name
//     inline def value[T](inline v: T): String = s"$v"
//     inline def equals[T](inline e1: String, inline e2: String): String =
//       s"($e1 == $e2)"

//   inline def expr[T](inline e: [E[_]] => TaglessDsl[E] => E[T]): Row => T =
//     e[RowFun](taglessEval)
