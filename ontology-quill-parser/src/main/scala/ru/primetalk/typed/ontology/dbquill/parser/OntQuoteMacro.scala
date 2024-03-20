package ru.primetalk.typed.ontology.dbquill.parser

import ru.primetalk.typed.ontology.simple.meta.TableBuilder
import ru.primetalk.typed.ontology.simple.meta.RecordSchemaValueType
import io.getquill.Quoted
import io.getquill.parser.ParserLibrary
import scala.quoted._
import scala.compiletime.summonInline
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
  #@,
  #:
}
import ru.primetalk.typed.ontology.utils.nameOf
import ru.primetalk.typed.ontology.simple.relalg.Relation
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType
import io.getquill.generic.GenericEncoder
import io.getquill.ast.PropertyAlias
import ru.primetalk.typed.ontology.simple.meta.EmptySchema

inline def ontQuote[T <: TableBuilder, V <: Tuple](
    tableBuilder: T
)(using
    inline svt: SchemaValueType[tableBuilder.TableSchema, V #@ tableBuilder.TableSchema]
): Quoted[EntityQuery[V #@ tableBuilder.TableSchema]] = ${
  OntQuoteMacro[T, tableBuilder.TableSchema, V]('tableBuilder, 'svt)
}

object OntQuoteMacro {
  def apply[T <: TableBuilder: Type, S <: RecordSchema: Type, V <: Tuple: Type](
      tableBuilderExpr: Expr[T],
      svt: Expr[SchemaValueType[S, V #@ S]]
  )(using Quotes): Expr[Quoted[EntityQuery[V #@ S]]] =
    import quotes.reflect.*
    val quat = Quat.Product.apply("my_product", Iterable.empty)
    val propertyAliases1 = Expr.ofList(propertyAliases[S, V #@ S]())
    // val e = Entity.Opinionated("order1", propertyAliases, quat, Renameable.Fixed)
    // import io.getquill.parser.Lifter.default.liftEntity
    // val expr = liftEntity(e)
    import io.getquill.parser.Lifter.default.liftQuat
    val quatExpr: Expr[Quat.Product] = liftQuat(quat).asInstanceOf[Expr[Quat.Product]]
    '{ Quoted(Entity.Opinionated("order1", $propertyAliases1, $quatExpr, Renameable.Fixed), Nil, Nil) }
  
  def propertyAliases[S <: SchemaLike: Type, T <: Tuple: Type](
      i: Int = 0,
      accum: List[Expr[PropertyAlias]] = Nil
  )(using Quotes): List[Expr[PropertyAlias]] =
    import quotes.reflect.*

    Type.of[S] match
      case '[EmptySchema] =>
        accum.reverse
      case '[p #: s] =>
        val name = TypeTree.of[p] match
          case SchemaBasedParserMacros.ColumnName(name) => name
          case _ => TypeTree.of[p].show
        propertyAliases[s, T](i + 1, propertyAlias(i, name) :: accum)

  /** Формируем переименования в соответствии с PropertyAliasExpr
    */
  def propertyAlias[T <: Tuple: Type](i: Int, name: String)(using
      Quotes
  ): Expr[PropertyAlias] =
    import quotes.reflect.*
    '{ PropertyAlias(List(${Expr(s"_${i+1}")}), ${Expr(name)})}
  
}
