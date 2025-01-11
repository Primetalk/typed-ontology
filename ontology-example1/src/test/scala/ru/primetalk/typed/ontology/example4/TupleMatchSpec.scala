//package ru.primetalk.typed.ontology.example4
//
//import scala.quoted.*
//
//class TupleMatchSpec extends BaseSpec:
//  test("tuple") {
//    Print.print(1 *: "" *: EmptyTuple)
//  }
//
//object Print:
//  inline def print[T <: Tuple](inline t: T): String = ${ PrintMacro.print[T]('t) }
