package ru.primetalk.typed.ontology.dbquill.parser

import io.getquill.querySchema
import scala.quoted.*
import ru.primetalk.typed.ontology.simple.meta.{#@, #:, EmptySchema, SchemaLike, SchemaValueType}
import io.getquill.EntityQuery
import ru.primetalk.typed.ontology.simple.meta.RecordSchema

object SchemaBasedParserMacros:
  def ontqueryImpl2[S <: RecordSchema: Type, T <: Tuple: Type](
      tableName: Expr[String]
  )(using Quotes): Expr[EntityQuery[T]] =
    import quotes.reflect.*

    val seqArgs = Expr.ofSeq(propertyAliases[S, T]())
    val args: List[Term] = propertyAliases[S, T]().map(_.asTerm)
    val tree = Apply(
      TypeApply(Ref(Symbol.requiredMethod("io.getquill.querySchema")), List(TypeTree.of[T])),
      List(
        tableName.asTerm,
        varArgTerm[T => (Any, String)](args)
      )
    )
    tree.asExprOf[EntityQuery[T]]

  def propertyAliases[S <: SchemaLike: Type, T <: Tuple: Type](
      i: Int = 0,
      accum: List[Expr[T => (Any, String)]] = Nil
  )(using Quotes): List[Expr[T => (Any, String)]] =
    import quotes.reflect.*

    Type.of[S] match
      case '[EmptySchema] =>
        accum.reverse
      case '[p #: s] =>
        val name = TypeTree.of[p] match
          case ColumnName(name) => name
          case _ => TypeTree.of[p].show
        propertyAliases[s, T](i + 1, propertyAlias(i, name) :: accum)

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
          val _2 = Literal(StringConstant(name)).asExprOf[String]
          '{ $_1 -> $_2 }.asTerm
        }
      )
    lambda.asExprOf[T => (Any, String)]

  /** Извлекаем имя колонки из типа свойства. */
  object ColumnName:

    def unapply(using Quotes)(t: quotes.reflect.TypeTree): Option[String] =
      import quotes.reflect.*
      t.tpe match
        case TermRef(typeRepr, name) => Some(name)
        case _ => Some("\"invalid_column_name_" +t.show +"\"")
  end ColumnName

  /** Make an instance of RepeatedParamClass that is used to represent var args. */
  def varArgTerm[T: Type](using Quotes)(args: List[quotes.reflect.Term]): quotes.reflect.Typed =
    import quotes.reflect.*
    Typed(
      Inlined(
        None,
        Nil,
        Repeated(
          args,
          TypeTree.of[T]
        )
      ),
      Applied(TypeIdent(defn.RepeatedParamClass), List(TypeTree.of[T]))
    )
end SchemaBasedParserMacros
