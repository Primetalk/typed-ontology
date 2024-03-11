package ru.primetalk.typed.ontology.dbquill.parser

import io.getquill.parser.ParserLibrary
import scala.quoted._
import io.getquill.{EntityQueryModel, EntityQuery, Unquoteable}
import io.getquill.metaprog.Extractors._
import io.getquill.parser.ParserHelpers._
import io.getquill.parser.QueryParser
import io.getquill.ast.{Ast, Entity, Renameable}
import io.getquill.quat.Quat
import io.getquill.parser.engine.ParserChain
import io.getquill.parser.engine.Parser
import io.getquill.quotation.NonQuotedException
import io.getquill.norm.TranspileConfig
import ru.primetalk.typed.ontology.simple.meta.{
  annotated,
  RecordSchema,
  RecordSchemaValueType,
  SchemaLike,
  SchemaValueType,
  TableBuilder,
  #@
}
import ru.primetalk.typed.ontology.simple.relalg.Relation
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType
import io.getquill.generic.GenericEncoder
import io.getquill.querySchema
// object OntEntityQuery {
//   def apply[S <: SchemaLike, T](tableName: String, svt: SchemaValueType[S, T]) =
//     new OntEntityQuery[S, T, svt.AValue](tableName, svt)
// }
import ru.primetalk.typed.ontology.simple.meta.SimpleTypes.{given, *}

class OntEntityQuery[S <: SchemaLike, T, AV](val tableName: String, val svt: SchemaValueType[S, T])
    extends EntityQuery[T] {
  // override def withFilter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  // override def filter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  // override def map[R](f: T => R): EntityQuery[R] = NonQuotedException()
}


object MyTestEntity
object MyTestEntity2

extension [T <: TableBuilder](t: T)
  inline def quillQuery(using rsvt: RecordSchemaValueType.Aux1[t.TableSchema]) =
    ontquery[rsvt.Schema, rsvt.Value, rsvt.AValue](t.tableName)(using recordSchemaValueType)

transparent inline def ontquery[S <: RecordSchema, T <: Tuple, AV <: T#@S](tableName: String)(using
    svt: SchemaValueType[S, T]
) = ${
  SchemaBasedParserMacros.ontqueryImpl2[S, T]('tableName)
}
  // querySchema[T](tableName)
  //   .map{t => 
  //     tupleConverter(t)
  //   }
  // new OntEntityQuery[S, T, AV](tableName, svt) // NonQuotedException()

class SchemaBasedParser(val rootParse: Parser)(using Quotes, TranspileConfig)
    extends Parser(rootParse)
    with Parser.PrefilterType[OntEntityQuery[?, ?, ?]]
    with PropertyAliases
    with Helpers {
  import quotes.reflect._
  // import quotes.reflect.{Constant => TConstant, Ident => TIdent, _}
  import MatchingOptimizers._

  // private def warnVerifyNoBranches(v: VerifyNoBranches.Output, expr: Expr[_]): Unit =
  //   if (v.messages.nonEmpty)
  //     report.warning("Questionable row-class found.\n" + v.messages.map(_.msg).mkString("\n"), expr)

  def attempt = {
    // case expr @ '{
    //   type s <: SchemaLike
    //   type t
    //   type av
    //   type overall <: OntEntityQuery[`s`, `t`, `av`]
    //   new OntEntityQuery[`s`, `t`, `av`]($name, $svt): OntEntityQuery[`s`, `t`, `av`]
    // } =>
    //   //error(expr)
    //   val quat          = Quat.Product.apply("unknown", Quat.Product.Type.Abstract, Iterable.empty[(String, Quat)])// InferQuat.ofType(tpe).probit
    //   Entity.Opinionated("name1", List(), quat, Renameable.Fixed)
    case expr @ '{
          type s <: RecordSchema
          type t
          type av
          //      type svtt <: SchemaValueType[s, t] : svtt}
          new OntEntityQuery[`s`, `t`, `av`]($name, $svt): OntEntityQuery[`s`, `t`, `av`]
        } =>
      // val svtV          = svtFromExpr[s, t].unapply(svt).getOrElse(error(svt))
      // val tpe           = TypeRepr.of[svtV.AValue]
      // val quat          = InferQuat.ofType(tpe).probit
      val quat          = Quat.Product.apply("unknown", Quat.Product.Type.Abstract, Iterable.empty[(String, Quat)])// InferQuat.ofType(tpe).probit
      val name1: String = FromExpr.StringFromExpr[String].unapply(name).getOrElse(error(name))
      // warnVerifyNoBranches(VerifyNoBranches.in(quat), expr)
      val res = Entity.Opinionated(name1, List(), quat, Renameable.Fixed)
      report.info(s"####SchemaBasedParser: $res")
      res

    // case expr @ '{
    //       type t
    //       type s <: RecordSchema
    //       type svtt// <: Expr[SchemaValueType[`s`, `t`]] // : svtt}
    //       ontquery[`s`, `t`]($name)(using ${ svt: svtt })
    //     } =>
    //   // val svtV          = svtFromExpr[s, t].unapply(svt).getOrElse(error(svt))
    //   val tpe           = TypeRepr.of[t #@ s]// TypeRepr.of[svtt] // V.AValue]
    //   // val tt = Type.of[t #@ s]
    //   // Quat.Product.Type.Abstract
    //   val quat          = Quat.Product.apply("unknown", Quat.Product.Type.Abstract, Iterable.empty[(String, Quat)])// InferQuat.ofType(tpe).probit
    //   val name1: String = FromExpr.StringFromExpr[String].unapply(name).getOrElse(error(name))
    //   // warnVerifyNoBranches(VerifyNoBranches.in(quat), expr)
    //   val res = Entity.Opinionated(name1, List(), quat, Renameable.Fixed)
    //   report.info(s"####SchemaBasedParser: $res")
    //   res
    // // report.info(s"####SchemaBasedParser.expr: ${expr.show}")
    // // throw IllegalArgumentException("123")
  }

}

object SchemaBasedParser extends ParserLibrary:
  import Parser._
  override def queryParser(using Quotes, TranspileConfig) =
    ParserChain.attempt(SchemaBasedParser(_)) orElse
      ParserChain.attempt(QueryParser(_))
  // (using svt: SchemaValueType[S, V])
  
  inline given svtGenericDecoder[S <: SchemaLike: Type, V <: Tuple: Type, ResultRow: Type, Session]
      : GenericDecoder[ResultRow, Session, TupleConverter[V] #@ S, DecodingType.Specific] =
    new:
      def apply(i: Int, rr: ResultRow, s: Session): TupleConverter[V] #@ S =
        var res: V | Null = null
        val a             = res.annotated[S]
        res.asInstanceOf[TupleConverter[V] #@ S]

  inline given svtGenericEncoder[S <: SchemaLike: Type, V <: Tuple: Type, PrepareRow: Type, Session]
      : GenericEncoder[TupleConverter[V] #@ S, PrepareRow, Session] =
    new:
      def apply(i: Int, t: TupleConverter[V] #@ S, row: PrepareRow, session: Session): PrepareRow =
        scala.compiletime.error("svtGenericEncoder not implemented")
