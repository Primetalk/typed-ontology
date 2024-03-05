package ru.primetalk.typed.ontology.simple.relalg

import cats.effect.kernel.Concurrent
import cats.Functor
import cats.FlatMap
import cats.Foldable
import cats.Traverse
import cats.SemigroupK
import cats.MonoidK
import cats.FunctorFilter
import cats.kernel.Semigroup
import cats.kernel.Order
import scala.collection.immutable.SortedMap
import cats.Applicative
import ru.primetalk.typed.ontology.simple.meta.ForeignKeyId0
import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.RecordProperty
import ru.primetalk.typed.ontology.simple.meta.RecordProperty0
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType
import ru.primetalk.typed.ontology.simple.meta.TableBuilder
import fs2._
import ru.primetalk.typed.ontology.simple.meta.RecordSchemaValueType

/** RelationF is a pair of schema and a stream of instances of that schema. F - is the effect type.
  */
abstract class RelationF[+F[_]] extends ExprClassicDsl:
  self =>

  type Schema <: RecordSchema
  val schema: Schema

  val svt: RecordSchemaValueType[Schema]// = summon[SchemaValueType[schema.type]]
  
  type Row = svt.Value

  val rows: Stream[F, Row]

  def show[F2[x] >: F[x]](using Concurrent[F2], Functor[F2]): F2[String] =
    import cats.syntax.functor.given
    val rowStrings: F2[List[String]] = rows
      .map(_.toString)
      .compile
      .toList
    val header = schema.toString
    rowStrings.map { rowStrings =>
      (header :: "".padTo(header.length, '-') :: rowStrings)
        .mkString("\n")
    }

//   transparent inline def projection[S2 <: RecordSchema, F2[x] >: F[x]](s2: S2)(using Functor[F2]) =
//     import cats.syntax.functor.given
//     val f    = s2.projectorFrom(schema)
//     val vals = rows.map(f)
//     RelationF(s2)(vals)

//   transparent inline def crossProductFrom[R1 <: RelationF[F2], F2[x] >: F[x]](r1: R1)(using
//       FlatMap[F2]
//   ): RelationF[F2] =
//     import cats.syntax.flatMap.given
//     val schema3 = r1.schema.appendOtherSchema(schema)
//     val concatValues: (r1.schema.Values, schema.Values) => schema3.Values =
//       r1.schema.appendValues(schema)(schema3)
//     val vals =
//       for
//         row1 <- r1.rows
//         row2 <- this.rows
//       yield concatValues(row1, row2)
//     RelationF(schema3)(vals)

//   transparent inline def crossProduct[R2 <: RelationF[F2], F2[x] >: F[x]](r2: R2)(using
//       FlatMap[F2]
//   ) =
//     import cats.syntax.flatMap.given
//     val schema3 = schema.appendOtherSchema(r2.schema)
//     val f: (schema.Values, r2.schema.Values) => schema3.Values =
//       schema.appendValues(r2.schema)(schema3)
//     val vals =
//       for
//         row1 <- this.rows
//         row2 <- r2.rows
//       yield f(row1, row2)
//     RelationF[schema3.type, F2](schema3)(vals)

//   transparent inline def join[FK <: ForeignKeyId0, R2 <: RelationF[F2], F2[x] >: F[x]](
//       inline fk: FK
//   )(r2: R2)(using FlatMap[F2])(using FunctorFilter[F2]) =
//     import cats.syntax.flatMap.given
//     import cats.syntax.functorFilter.given
//     val schema3 = schema.appendOtherSchema(r2.schema)
//     val concatValues: (schema.Values, r2.schema.Values) => schema3.Values =
//       schema.appendValues(r2.schema)(schema3)
//     val pred: schema3.Values => Boolean = schema3.fkPredicate(fk)
//     val vals =
//       for
//         row1 <- this.rows
//         row2 <- r2.rows
//         row3 = concatValues(row1, row2)
//       // if pred(row3)
//       yield row3
//     val filtered = vals.filter(pred)
//     RelationF[schema3.type, F2](schema3)(filtered)

//   transparent inline def prependCalcColumn[P <: RecordProperty0, F2[x] >: F[x]](p: P)(
//       inline f: Row => p.P
//   )(using FlatMap[F2]) =
//     import cats.syntax.flatMap.given
//     val schema3 = p #: schema
//     val vals    = rows.map(row => (f(row) *: row).asInstanceOf[schema3.Values])
//     RelationF(schema3)(vals)

//   transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T], F2[x] >: F[x]](
//       inline p1: P1,
//       p2: P2
//   )(using Functor[F2]) =
//     val schema3 = schema.rename(p1, p2)
//     val vals    = rows.map(_.asInstanceOf[schema3.Values])
//     RelationF[schema3.type, F2](schema3)(vals)

//   transparent inline def ++[R2 <: RelationF[F2], F2[x] >: F[x]](r2: R2)(using
//       ev: r2.schema.Values =:= schema.Values
//   )(using SemigroupK[F2])(using Functor[F2]) =
//     import cats.syntax.all.toSemigroupKOps
//     val rows1: Stream[F2, schema.Values] = rows // .map(identity)
//     val vals: Stream[F2, schema.Values]  = rows1 <+> r2.rows.map(ev)
//     RelationF[schema.type, F2](schema)(vals)

//   transparent inline def replaceRows[F2[x] >: F[x]](inline f: Stream[F, Row] => Stream[F2, Row]) =
//     RelationF(schema)(f(rows))

//   transparent inline def filter[F2[x] >: F[x]](inline predicate: Row => Boolean)(using
//       FunctorFilter[F2]
//   ) =
//     import cats.syntax.functorFilter.given
//     replaceRows(_.filter(predicate))
//   // TODO: try optimizing withFilter
//   transparent inline def withFilter[F2[x] >: F[x]](inline predicate: Row => Boolean)(using
//       FunctorFilter[F2]
//   ) =
//     import cats.syntax.functorFilter.given
//     replaceRows(_.filter(predicate))

//   transparent inline def filterNot[F2[x] >: F[x]](inline predicate: Row => Boolean)(using
//       FunctorFilter[F2]
//   ) =
//     import cats.syntax.functorFilter.given
//     replaceRows(_.filterNot(predicate))

//   /** NB: O(N + M ln M), N = rows.size, M = R2.rows.size */
//   transparent inline def --[R2 <: RelationF[F2], F2[x] >: F[x]](r2: R2)(using
//       ev: r2.schema.Values =:= schema.Values
//   )(using Functor[F2])(using FunctorFilter[F2])(using Concurrent[F2]): F2[Any] =
//     Functor[F2].map(r2.rows.compile.toVector) { vector =>
//       val set2 = vector.toSet.map(ev)
//       filterNot[F2](set2.contains)
//     }

//   // final def groupAdjacentBy[B, F2[x] >: F[x]](f: Row => B)(using Order[B])(using Foldable[F2])(using MonoidK[F2])(using Applicative[F2]): SortedMap[B, V[Row]] =
//   //   groupMap(key = f)(identity)

//   // final def groupAdjacentMap[K, B, F2[x] >: F[x]](key: Row => K)(f: Row => B)(using K: Order[K])(using Foldable[F2])(using SemigroupK[F2])(using Applicative[F2]): SortedMap[K, V[B]] =
//   //   given ordering: Ordering[K] = K.toOrdering
//   //   import cats.syntax.all.toFoldableOps
//   //   import cats.syntax.all.toSemigroupKOps
//   //   val app = Applicative[F2]
//   //   rows.foldLeft(SortedMap.empty[K, V[B]])( (m, elem) =>
//   //     val k = key(elem)
//   //     m.get(k) match {
//   //       case Some(b) => m.updated(key = k, value = b <+> app.pure(f(elem)))
//   //       case None    => m + (k -> app.pure(f(elem)))
//   //     }
//   //   )
// //   final def groupMapReduce[K, B](key: Row => K)(f: Row => B)(using K: Order[K], S: Semigroup[B])(using Foldable[F2]): SortedMap[K, B] =
// //     groupMapReduceWith(key)(f)(S.combine)

// //   final def groupMapReduceWith[K, B](key: Row => K)(f: Row => B)(combine: (B, B) => B)(using K: Order[K])(using Foldable[F2]): SortedMap[K, B] =
// //     given ordering: Ordering[K] = K.toOrdering
// //     import cats.syntax.all.toFoldableOps

// //     rows.foldLeft(SortedMap.empty[K, B])( (m, elem) =>
// //       val k = key(elem)

// //       m.get(k) match {
// //         case Some(b) => m.updated(key = k, value = combine(b, f(elem)))
// //         case None    => m + (k -> f(elem))
// //       }

// //     )
//   def toSemigroup[A](combineImpl: (A, A) => A): Semigroup[A] =
//     new Semigroup[A]:
//       def combine(a: A, b: A): A = combineImpl(a, b)

// //   transparent inline def groupMapReduceS[
// //     KeySchema <: RecordSchema,
// //     AggregateSchema <: RecordSchema
// //     ](
// //     inline keySchema: KeySchema,
// //     inline aggregateSchema: AggregateSchema
// //     )(
// //       inline resultSchema: RecordSchema.Concat[keySchema.type, aggregateSchema.type],//inline schema3: RecordSchema.Concat[this.type, schema2.type]
// //     )(
// //       inline k: Row => keySchema.Values,
// //       inline m: Row => aggregateSchema.Values
// //     )(using Order[keySchema.Values])
// //     (using Semigroup[aggregateSchema.Values])// to aggregate values, something like `sum`
// //     (using MonoidK[F2])
// //     (using Applicative[F2])
// //     (using Foldable[F2])
// //     // : RelationF[F2], F2[x] >: F[x]{
// //     //   // type Schema = resultSchema.type
// //     // }
// //      =
// //       // (
// //       // val resultSchema = keySchema.concat(aggregateSchema)// : RecordSchema.Concat[keySchema.type, aggregateSchema.type]
// //       // )
// //       val grouped = groupMapReduce[keySchema.Values, aggregateSchema.Values](k)(m)

// //       convertSortedMapToRelation(keySchema, aggregateSchema)(resultSchema)(grouped)
// //       // val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
// //       // // concat
// //       // val allVals: Iterable[resultSchema.Values] = grouped.toIterable.map(concat(_, _))
// //       // import cats.MonoidK.ops.toAllMonoidKOps
// //       // val vals = allVals.foldLeft(MonoidK[F2].empty[resultSchema.Values])((b, a) => b <+> Applicative[F2].pure(a))
// //       // Relation.apply(resultSchema)(vals)
// transparent inline def convertSortedMapToRelationF[
//     F2[_],
//     KeySchema <: RecordSchema,
//     AggregateSchema <: RecordSchema
//     // ResultSchema <: RecordSchema.Concat[KeySchema, AggregateSchema]
// ](
//     keySchema: KeySchema,
//     aggregateSchema: AggregateSchema
// )(
//     resultSchema: RecordSchema.Concat[
//       keySchema.type,
//       aggregateSchema.type
//     ] // inline schema3: RecordSchema.Concat[this.type, schema2.type]
// )(grouped: SortedMap[keySchema.Values, aggregateSchema.Values])(using Order[keySchema.Values])(using
//     Semigroup[aggregateSchema.Values]
// ) // to aggregate values, something like `sum`
// (using MonoidK[F2])(using Applicative[F2])(using Foldable[F2]): RelationF[F2] {
//   type Schema = resultSchema.type
// } =
//   val concat                                = keySchema.concatValues(aggregateSchema)(resultSchema)
//   val vals: Stream[F2, resultSchema.Values] = Stream.emits(grouped.toSeq.map(concat(_, _)))
//   RelationF.apply[RecordSchema.Concat[keySchema.type, aggregateSchema.type], F2](resultSchema)(vals)

// transparent inline def convertSortedMapToVF[
//     F2[_],
//     KeySchema <: RecordSchema,
//     AggregateSchema <: RecordSchema
//     // ResultSchema <: RecordSchema.Concat[KeySchema, AggregateSchema]
// ](
//     keySchema: KeySchema,
//     aggregateSchema: AggregateSchema
// )(grouped: SortedMap[keySchema.Values, aggregateSchema.Values])(using Order[keySchema.Values])(using
//     Semigroup[aggregateSchema.Values]
// ) // to aggregate values, something like `sum`
// (using MonoidK[F2])(using Applicative[F2])(using Foldable[F2]) =
//   val resultSchema =
//     keySchema.concat(aggregateSchema) // : RecordSchema.Concat[keySchema.type, aggregateSchema.type]
//   val concat                                 = keySchema.concatValues(aggregateSchema)(resultSchema)
//   val allVals: Iterable[resultSchema.Values] = grouped.toSeq.map(concat(_, _))
//   val vals = allVals.foldLeft(MonoidK[F2].empty[resultSchema.Values])((b, a) =>
//     MonoidK[F2].combineK(b, Applicative[F2].pure(a))
//   )
//   vals

// object RelationF:
//   transparent inline def apply[S1 <: RecordSchema, F[_]](s1: S1)(inline v: Stream[F, s1.Values]) =
//     new RelationF[F] {
//       type Schema = s1.type
//       val schema = s1
//       val rows   = v
//     }

//   transparent inline def empty[S1 <: RecordSchema, F2[x]](s1: S1)(using MonoidK[F2]) =
//     apply[s1.type, F2](s1)(Stream.empty)

//   type RelationOf[S <: RecordSchema] = [F[_]] =>> RelationF[F] {
//     type Schema = S
//   }
//   type RelationOfV[F2[_]] = [S <: RecordSchema] =>> RelationF[F2] {
//     type Schema = S
//   }

// extension (tb: TableBuilder)
//   transparent inline def relationF[F[_]](inline values1: Stream[F, tb.Row]): RelationF[F] =
//     RelationF(tb.tableSchema)(values1)
