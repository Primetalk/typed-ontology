package ru.primetalk.typed.ontology.dbquill.parser

import io.getquill.ast.{Ident => AIdent, Query => AQuery, Action => AAction, Insert => AInsert, Update => AUpdate, Delete => ADelete, _}
import io.getquill.ast
import io.getquill.metaprog.PlanterExpr
import io.getquill.metaprog.QuotedExpr
import scala.quoted._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable

import scala.reflect.ClassTag
import io.getquill.norm.capture.AvoidAliasConflict
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.util.Format
import io.getquill.parser.ParserHelpers._
import io.getquill.quat.QuatMaking
import io.getquill.quat.QuatMakingBase
import io.getquill.quat.Quat
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.Uprootable
import io.getquill.metaprog.Pluckable
import io.getquill.metaprog.Pointable
import io.getquill.metaprog.Extractors._
import io.getquill.metaprog.SummonTranspileConfig
import io.getquill._

import io.getquill.Ord
import io.getquill.Embedded
import io.getquill.metaprog.Is
import io.getquill.generic.ElaborationSide
import io.getquill.parser.engine._
import io.getquill.context.VerifyFreeVariables
import io.getquill.norm.TranspileConfig
import io.getquill.ast.External.Source
import io.getquill.quat.VerifyNoBranches

import io.getquill.parser.ParserLibrary
import ru.primetalk.typed.ontology.simple.meta.{#@, SchemaLike}
import ru.primetalk.typed.ontology.utils.nameOf
import ru.primetalk.typed.ontology.simple.relalg.Relation
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType
import io.getquill.generic.GenericEncoder
import io.getquill.querySchema
import ru.primetalk.typed.ontology.simple.meta.AnnotatedTypesContext.{given, *}

class OntologyQueryParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[Query[_]]
    with PropertyAliases
    with Helpers {
  import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}
  import MatchingOptimizers._

  private def warnVerifyNoBranches(v: VerifyNoBranches.Output, expr: Expr[_]): Unit =
    if (v.messages.nonEmpty)
      report.warning("Questionable row-class found.\n" + v.messages.map(_.msg).mkString("\n"), expr)

  def attempt = {
    case expr @ '{ type t; EntityQuery.apply[`t`] } =>
      val tpe = TypeRepr.of[t]
      val name: String = tpe.classSymbol.get.name
      val quat = InferQuat.ofType(tpe).probit
      warnVerifyNoBranches(VerifyNoBranches.in(quat), expr)
      Entity(name, List(), quat)

    case expr @ '{ querySchema[t](${ ConstExpr(name: String) }, ${ GenericSeq(properties) }: _*) } =>
      val quat = InferQuat.of[t].probit
      warnVerifyNoBranches(VerifyNoBranches.in(quat), expr)
      Entity.Opinionated(name, properties.toList.map(PropertyAliasExpr.OrFail[t](_)), quat, Renameable.Fixed)

    case "map" -@> '{ ($q: Query[qt]).map[mt](${ Lambda1(ident, tpe, body) }) } =>
      Map(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "flatMap" -@> '{ ($q: Query[qt]).flatMap[mt](${ Lambda1(ident, tpe, body) }) } =>
      FlatMap(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "filter" -@> '{ ($q: Query[qt]).filter(${ Lambda1(ident, tpe, body) }) } =>
      Filter(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "withFilter" -@> '{ ($q: Query[qt]).withFilter(${ Lambda1(ident, tpe, body) }) } =>
      Filter(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "concatMap" -@@> '{ type t1; type t2; ($q: Query[qt]).concatMap[`t1`, `t2`](${ Lambda1(ident, tpe, body) })($unknown_stuff) } => // ask Alex why is concatMap like this? what's unkonwn_stuff?
      ConcatMap(rootParse(q), cleanIdent(ident, tpe), rootParse(body))

    case "union" -@> '{ ($a: Query[t]).union($b) }       => Union(rootParse(a), rootParse(b))
    case "unionAll" -@> '{ ($a: Query[t]).unionAll($b) } => UnionAll(rootParse(a), rootParse(b))
    case "++" -@> '{ ($a: Query[t]).++($b) }             => UnionAll(rootParse(a), rootParse(b))

    case ("join" -@> '{ type t1; type t2; ($q1: Query[`t1`]).join[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(InnerJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))
    case ("leftJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).leftJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(LeftJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))
    case ("rightJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).rightJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(RightJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))
    case ("fullJoin" -@> '{ type t1; type t2; ($q1: Query[`t1`]).fullJoin[`t1`, `t2`](($q2: Query[`t2`])) }) withOnClause(OnClause(ident1, tpe1, ident2, tpe2, on)) =>
      Join(FullJoin, rootParse(q1), rootParse(q2), cleanIdent(ident1, tpe1), cleanIdent(ident2, tpe2), rootParse(on))

    case "join" -@> '{ type t1; ($q1: Query[`t1`]).join[`t1`](${ Lambda1(ident1, tpe, on) }) } =>
      FlatJoin(InnerJoin, rootParse(q1), cleanIdent(ident1, tpe), rootParse(on))
    case "leftJoin" -@> '{ type t1; ($q1: Query[`t1`]).leftJoin[`t1`](${ Lambda1(ident1, tpe, on) }) } =>
      FlatJoin(LeftJoin, rootParse(q1), cleanIdent(ident1, tpe), rootParse(on))

    case "take" -@> '{ type t; ($q: Query[`t`]).take($n: Int) } => Take(rootParse(q), rootParse(n))
    case "drop" -@> '{ type t; ($q: Query[`t`]).drop($n: Int) } => Drop(rootParse(q), rootParse(n))

    // 2-level so we don't care to select-apply it now
    case "sortBy" -@@> '{ type r; ($q: Query[t]).sortBy[`r`](${ Lambda1(ident1, tpe, body) })($ord: Ord[`r`]) } =>
      SortBy(rootParse(q), cleanIdent(ident1, tpe), rootParse(body), rootParse(ord))

    case "groupBy" -@> '{ type r; ($q: Query[t]).groupBy[`r`](${ Lambda1(ident1, tpe, body) }) } =>
      GroupBy(rootParse(q), cleanIdent(ident1, tpe), rootParse(body))

    case "groupByMap" -@@> '{ ($q: Query[t]).groupByMap[g, r](${ Lambda1(byIdent, byTpe, byBody) })(${ Lambda1(mapIdent, mapTpe, mapBody) }) } =>
      GroupByMap(rootParse(q), cleanIdent(byIdent, byTpe), rootParse(byBody), cleanIdent(mapIdent, mapTpe), rootParse(mapBody))

    case "distinctOn" -@> '{ ($q: Query[t]).distinctOn[r](${ Lambda1(ident, tpe, body) }) } =>
      rootParse(q) match {
        case fj: FlatJoin => failFlatJoin("distinctOn")
        case other        => DistinctOn(rootParse(q), cleanIdent(ident, tpe), rootParse(body))
      }

    case "distinct" --> '{ ($q: Query[t]).distinct } =>
      rootParse(q) match {
        case fj: FlatJoin => failFlatJoin("distinct")
        case other        => Distinct(other)
      }

    case "nested" --> '{ ($q: Query[t]).nested } =>
      rootParse(q) match {
        case fj: FlatJoin => failFlatJoin("nested")
        case other        => io.getquill.ast.Nested(other)
      }
  }

  def failFlatJoin(clauseName: String) =
    report.throwError(
      s"""
        |The .${clauseName} cannot be placed after a join clause in a for-comprehension. Put it before.
        |For example. Change:
        |  for { a <- query[A]; b <- query[B].join(...).nested } to:
        |  for { a <- query[A]; b <- query[B].nested.join(...) }
        |""".stripMargin
    )

  import io.getquill.JoinQuery

  private case class OnClause(ident1: String, tpe1: quotes.reflect.TypeRepr, ident2: String, tpe2: quotes.reflect.TypeRepr, on: quoted.Expr[_])
  private object withOnClause {
    def unapply(jq: Expr[_]) =
      jq match {
        case '{ ($q: JoinQuery[a, b, r]).on(${ Lambda2(ident1, tpe1, ident2, tpe2, on) }) } =>
          Some((UntypeExpr(q), OnClause(ident1, tpe1, ident2, tpe2, on)))
        case _ =>
          None
      }
  }

} // end QueryParser


object OntologyQueryParser extends ParserLibrary:
  import Parser._
  override def queryParser(using Quotes, TranspileConfig) =
    ParserChain.attempt(OntologyQueryParser(_))
  // (using svt: SchemaValueType[S, V])

  inline given svtGenericDecoder[S <: SchemaLike, V <: Tuple, ResultRow, Session]
      : GenericDecoder[ResultRow, Session, V #@ S, DecodingType.Specific] =
    new:
      def apply(i: Int, rr: ResultRow, s: Session): V #@ S =
        var res: V | Null = null
        val a             = res.#@[S]
        res.asInstanceOf[V #@ S]

  inline given svtGenericEncoder[S <: SchemaLike, V <: Tuple, PrepareRow, Session]
      : GenericEncoder[V #@ S, PrepareRow, Session] =
    new:
      def apply(i: Int, t: V #@ S, row: PrepareRow, session: Session): PrepareRow =
        scala.compiletime.error("svtGenericEncoder not implemented")
