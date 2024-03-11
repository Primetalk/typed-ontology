package ru.primetalk.typed.ontology.dbquill.parser

import io.getquill.querySchema
import scala.quoted.*
import ru.primetalk.typed.ontology.simple.meta.{#@, #:, EmptySchema, SchemaLike, SchemaValueType}
import io.getquill.EntityQuery

object SchemaBasedParserMacros:
  def ontqueryImpl[S <: SchemaLike: Type, T <: Tuple: Type, AV <: T #@ S: Type](
      tableName: Expr[String],
      svt: Expr[SchemaValueType[S, T]]
  )(using Quotes) =
    import quotes.reflect.*
    val tpe = Type.of[S]
    tpe match
      case '[p #: s] =>
        val seqArgs = Expr.ofSeq(propertyAliases[S, T]())
        Apply(
          TypeApply(Ref(Symbol.requiredMethod("io.getquill.querySchema")), List(TypeTree.of[T])),
          List(
            tableName.asTerm,
            Typed(
              Inlined(
                None,
                Nil,
                Repeated(
                  // List(Literal(IntConstant(10)), Literal(StringConstant("str")), Literal(DoubleConstant(5.2))),
                  propertyAliases[S, T]().map(_.asTerm),
                  TypeTree.of[T => (Any, String)]
                )
              ),
              Applied(TypeIdent(defn.RepeatedParamClass), List(TypeTree.of[T => (Any, String)]))
            )
          )
        ).asExprOf[EntityQuery[T]]
      // '{
      //     querySchema[T]($tableName, ${seqArgs}*)
      // }

  def propertyAliases[S <: SchemaLike: Type, T <: Tuple: Type](
      i: Int = 0,
      accum: List[Expr[T => (Any, String)]] = Nil
  )(using Quotes): List[Expr[T => (Any, String)]] =
    import quotes.reflect.*

    Type.of[S] match
      case '[EmptySchema] =>
        accum.reverse
      case '[p #: s] =>
        propertyAliases[s, T](i + 1, propertyAlias(i, s"column$i") :: accum)

  /** Формируем переименования в соответствии с PropertyAliasExpr
    */
  def propertyAlias[T <: Tuple: Type](i: Int, name: String)(using
      Quotes
  ): Expr[T => (Any, String)] =
    import quotes.reflect.*
    val mtpe = MethodType(List("t"))(_ => List(TypeRepr.of[T]), _ => TypeRepr.of[(Any, String)])
    val tuple2term = TermRef.apply(TypeRepr.of[Tuple2[Any, String]], "Tuple2")
    val lambda = // Block.apply(List(),
      Lambda(
        Symbol.spliceOwner,
        mtpe,
        { case (methSym, List(arg1: Term)) =>
          val _1 = Select(arg1, Symbol.requiredMethod(s"_${i + 1}")).asExprOf[Any]
          val _2 = Literal(StringConstant(s"column${i + 1}")).asExprOf[String]
          val tu = makeTuple2OfAnyString(_1, _2)
          '{ $_1 -> $_2 }.asTerm
        }
      )
    lambda.asExprOf[T => (Any, String)]
