package ru.primetalk.typed.ontology.simple.relalg

import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.RecordProperty0

trait ExprClassicDsl:
  type Schema <: RecordSchema
  val schema: Schema
  type Row = schema.Values
  // DSL. It is part of a single relation to have intrinsic access to schema

  sealed trait RelExpr[T]
  case class Getter[T](name: String, f: Row => T) extends RelExpr[T]
  case class Function1Expr[A, B](r1: RelExpr[A], name: String, op: A => B) extends RelExpr[B]
  case class Function2Expr[A, B, C](r1: RelExpr[A], r2: RelExpr[B], name: String, op: (A, B) => C) extends RelExpr[C]

  inline def prop[P <: RecordProperty0](inline p: P): Getter[RecordProperty0.PropertyValueType[p.type]] =
    Getter(p.name, schema.propertyGetter(p))

  inline def const[T](inline t: T): Getter[T] = Getter(s"$t", _ => t)

  extension [T](r: RelExpr[T])
    inline def ===(inline other: RelExpr[T]): Function2Expr[T, T, Boolean] = 
      Function2Expr[T, T, Boolean](r, other, "==", _ == _)
    inline def map[R](inline f: T => R): Function1Expr[T, R] = 
      Function1Expr(r, "<f-map>", f)

  inline def not(r: RelExpr[Boolean]): Function1Expr[Boolean, Boolean] = 
    Function1Expr[Boolean, Boolean](r, "not", !_)
  extension (r: RelExpr[Boolean])
    inline def and(inline other: RelExpr[Boolean]): Function2Expr[Boolean, Boolean, Boolean] = 
      Function2Expr[Boolean, Boolean, Boolean](r, other, "and", _ && _)
    inline def or(inline other: RelExpr[Boolean]): Function2Expr[Boolean, Boolean, Boolean] = 
      Function2Expr[Boolean, Boolean, Boolean](r, other, "or", _ || _)

  extension [N: Numeric](r: RelExpr[N])
    inline def +(inline other: RelExpr[N]): Function2Expr[N, N, N] = 
      Function2Expr[N, N, N](r, other, "+", summon[Numeric[N]].plus)
    inline def *(inline other: RelExpr[N]): Function2Expr[N,N,N] = 
      Function2Expr[N,N,N](r, other, "*", summon[Numeric[N]].times)
  extension (r: RelExpr[String])
    inline def +(inline other: RelExpr[String]): Function2Expr[String,String,String] = 
      Function2Expr(r, other, "+", (s1, s2) => s1 + s2)
  inline def rowFun[T](inline expr: RelExpr[T]): Row => T =
    inline expr match
      case Getter(_, f)                => f
      case Function1Expr(n1, _, f)     => (r: Row) => f(rowFun(n1)(r))
      case Function2Expr(n1, n2, _, f) => (r: Row) => f(rowFun(n1)(r), rowFun(n2)(r))

  inline def show[T](inline expr: RelExpr[T]): String =
    inline expr match
      case Getter(name, _)                => name
      case Function1Expr(e1, name, _)     => s"$name(${show(e1)})"
      case Function2Expr(e1, e2, name, _) => s"(${show(e1)} $name ${show(e2)})"
