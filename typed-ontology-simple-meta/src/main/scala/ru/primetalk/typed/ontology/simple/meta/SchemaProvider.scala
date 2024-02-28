package ru.primetalk.typed.ontology.simple.meta

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.*
// import quotidian.*
/** Type class, позволяющий сгенерировать RecordSchema на основе case class'а. Неприятной
  * особенностью является отсутствие имён свойств в scope'е. Можно воспользоваться библиотекой quoti
  */
sealed trait SchemaProvider[T]:
  type Schema <: SchemaLike
  val schema: Schema
sealed trait TupleSchemaProvider[T] extends SchemaProvider[T]:
  type Schema <: TupleSchema
  val schema: Schema

object TupleSchemaProvider:
  def apply[T](using s: TupleSchemaProvider[T]): TupleSchemaProvider[T] = s

object SchemaProvider {
  def apply[T](using s: SchemaProvider[T]): SchemaProvider[T] = s

  given TupleSchemaProvider[EmptyTuple] = new {
    type Schema = EmptyTupleSchema.type
    val schema: Schema = EmptyTupleSchema
  }
  given SchemaProvider[Boolean] = new {
    type Schema = ScalarSchema.BooleanScalarSchema.type
    val schema = ScalarSchema.BooleanScalarSchema
  }
  given SchemaProvider[Int] = new {
    type Schema = ScalarSchema.IntScalarSchema.type
    val schema = ScalarSchema.IntScalarSchema
  }
  given SchemaProvider[String] = new {
    type Schema = ScalarSchema.StringScalarSchema.type
    val schema = ScalarSchema.StringScalarSchema
  }

  given [H: SchemaProvider, T <: Tuple: TupleSchemaProvider]: TupleSchemaProvider[H *: T] =
    val hs = SchemaProvider[H]
    val ts = TupleSchemaProvider[T]
    new {
      type Schema = NonEmptyTupleSchema[hs.Schema, ts.Schema] // SchemaProvider.apply[H].Schema
      val schema = NonEmptyTupleSchema[hs.Schema, ts.Schema](hs.schema, ts.schema)
    }
  def derived[T <: Product](using
      m: Mirror.ProductOf[T],
      elems: TupleSchemaProvider[m.MirroredElemTypes],
      caseClassMeta: CaseClassMeta[T]
  ): SchemaProvider[T] =
    new SchemaProvider[T] {
      type Schema =
        RecordSchema.RecordSchemaFromTupleSchema[T, elems.schema.Schemas, m.MirroredElemLabels]
      val properties =
        elems.schema.schemas.toIArray.zip(caseClassMeta.fields).map { case (s, name) =>
          new SchemaBasedPropertyId[T, SchemaLike](name, s.asInstanceOf[SchemaLike]) {}
        }
      val schema: Schema = properties
        .foldRight(EmptySchema: RecordSchema) { case (p, s) =>
          p #: s
        }
        .asInstanceOf[Schema]
    }

}

// summon[m.Mirrsummon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]osummon[m.MirroredElemTypes] summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]summon[m.MirroredElemTypes]redElemTypes]
// ${ derivedImpl[T]('m)}
// new RecordSchemaProvider[T]:
// type Schema = Nothing
// def schema: Schema = ???

// def derivedImpl[T <: Product : Type](m: Expr[Mirror.ProductOf[T]])(using Quotes): Expr[RecordSchemaProvider[T]] =

//   import quotes.*, quotes.reflect.*
//   import quotes.reflect.report.*
//   '{
//     val labels = constValue[FromExpr[Mirror.ProductOf[T]](m).get.MirroredLabel]
//     new RecordSchemaProvider[T]:
//       ${
//         m match
//           case Varargs(a) =>
//             errorAndAbort("Vararg")
//           case _ =>
//             errorAndAbort(s"Some expr (${m.show})", m)
//       }
//       type Schema = Nothing
//       def schema: Schema = ???
//   }
