package ru.primetalk.typed.ontology.simple.relalg

import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.RecordProperty0

trait PredicateClassicDsl:
  
  type Schema <: RecordSchema
  val schema: Schema
  type Row = schema.Values
  // DSL. It is part of a single relation to have intrinsic access to schema

  sealed trait RelExpr[T]
  case class Getter[T](name: String, f: Row => T) extends RelExpr[T]
  // case class Property[P <: RecordProperty0](p: P) extends RelExpr[p.P]
  case class Value[V](v: V) extends RelExpr[V]
  case class Equals[T](e1: RelExpr[T], e2: RelExpr[T]) extends RelExpr[Boolean]

  inline def prop[P <: RecordProperty0](inline p: P): Getter[RecordProperty0.PropertyValueType[p.type]] =
    Getter(p.name, schema.propertyGetter(p))
  extension [T](r: RelExpr[T])
    inline def ===(inline other: RelExpr[T]): Equals[T] = 
      Equals[T](r, other)
  inline def rowFun[T](inline expr: RelExpr[T]): Row => T =
    inline expr match
      case Value(v)       => (r: Row) => v
      case Getter(_, f)      => f
      // case Property(p)    => 
      //   val getter = schema.propertyGetter(p)
      //   (r: Row) => getter(r).asInstanceOf[T]
      case Equals(e1, e2) => (r: Row) => rowFun(e1)(r) == rowFun(e2)(r)
  inline def show[T](inline expr: RelExpr[T]): String =
    inline expr match
      case Value(v)       => s"$v"
      case Getter(name, _)      => name
      case Equals(e1, e2) => s"(${show(e1)} == ${show(e2)})"
