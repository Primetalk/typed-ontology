package ru.primetalk.typed.ontology.simple.relalg

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
import ru.primetalk.typed.ontology.simple.meta.{Concatenator, ForeignKeyId0, Projector, ##:}
import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.RecordProperty
import ru.primetalk.typed.ontology.simple.meta.RecordProperty0
import ru.primetalk.typed.ontology.simple.meta.TableBuilder
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType
import ru.primetalk.typed.ontology.simple.meta.RecordSchemaValueType
import cats.syntax.flatMap.given
import cats.syntax.functor.given
import cats.syntax.foldable.given

/** Relation is a pair of schema and a collection of instances of that schema. V - is the collection
  * type (List, Stream[...]).
  */
abstract class Relation[S <: RecordSchema, VS, V[_]](val schema: S)(using SchemaValueType[S, VS]) extends ExprClassicDsl:
  self =>

  type Schema = S

  val svt: SchemaValueType[S, VS] = summon[SchemaValueType[S, VS]]
  
  type Row = VS
  val rows: V[Row]
  type Self = Relation[S, VS, V] {
    type Schema = self.Schema
    type Row    = self.Row
  }
  def show(using Foldable[V]) =

    schema.toString +
      "\n-----\n" +
      rows
        .foldLeft(List[String]())((lst, row) => row.toString :: lst)
        .reverse
        .mkString("\n")

  transparent inline def projection[S2 <: RecordSchema, VS2 <: Tuple](s2: S2)(
    using
      proj: Projector[Schema, Row, S2, VS2],
      f: Functor[V],
    ) =
    val rsvt: SchemaValueType[S2, VS2] = proj.to
    val vals = rows.map(v => proj(v).asInstanceOf[rsvt.Value])
    Relation[S2, VS2, V](s2)(using rsvt)(vals)

  /**
    * Строим декартово произведение отношения r1 и текущего отношения.
    */
  transparent inline def crossProductFrom[S1 <: RecordSchema, VS1, R1 <: Relation[S1, VS1, V], VRes](r1: R1)(using
      fm: FlatMap[V],
      concat: Concatenator[r1.schema.type, VS1, this.schema.type, VS, VRes]
  )(using 
    ev1: r1.Row =:= concat.aSvt.Value,
    ev2: Row =:= concat.bSvt.Value
  ): Relation[concat.Schema, concat.abSvt.Value, V] =
    type S = concat.Schema
    val schema3: S = concat.schemaConcat(r1.schema, schema)
    val vals =
      for
        row1 <- r1.rows
        row2 <- this.rows
      yield concat(row1, row2)
    Relation[concat.Schema, concat.abSvt.Value, V](schema3: S)(using concat.abSvt)(vals)

  // transparent inline def crossProduct[R2 <: Relation[V]](r2: R2)(using FlatMap[V]) =
  //   import cats.syntax.flatMap.given
  //   import cats.syntax.functor.given
  //   val schema3 = schema.appendOtherSchema(r2.schema)
  //   val f: (schema.Values, r2.schema.Values) => schema3.Values =
  //     schema.appendValues(r2.schema)(schema3)
  //   val vals =
  //     for
  //       row1 <- this.rows
  //       row2 <- r2.rows
  //     yield f(row1, row2)
  //   Relation[schema3.type, V](schema3)(vals)

  // transparent inline def join[FK <: ForeignKeyId0, R2 <: Relation[V]](inline fk: FK)(r2: R2)(using
  //     FlatMap[V]
  // )(using FunctorFilter[V]) =
  //   import cats.syntax.flatMap.given
  //   import cats.syntax.functor.given
  //   import cats.syntax.functorFilter.given
  //   val schema3 = schema.appendOtherSchema(r2.schema)
  //   val concatValues: (schema.Values, r2.schema.Values) => schema3.Values =
  //     schema.appendValues(r2.schema)(schema3)
  //   val pred: schema3.Values => Boolean = schema3.fkPredicate(fk)
  //   val vals =
  //     for
  //       row1 <- this.rows
  //       row2 <- r2.rows
  //       row3 = concatValues(row1, row2)
  //     // if pred(row3)
  //     yield row3
  //   val filtered = vals.filter(pred)
  //   Relation[schema3.type, V](schema3)(filtered)

  // transparent inline def prependCalcColumn[P <: RecordProperty0](p: P)(using sv: SchemaValueType[p.P])(inline f: Row => sv.Value)(using
  //     FlatMap[V]
  // ) =
  //   import cats.syntax.flatMap.given
  //   import cats.syntax.functor.given
  //   val schema3 = p #: schema
  //   val vals    = rows.map(row => (f(row) *: row).asInstanceOf[schema3.Values])
  //   Relation(schema3)(vals)
  // transparent inline def prependCalcColumnF[P <: RecordProperty0](p: P)(inline f: RelExpr[p.P])(
  //     using FlatMap[V]
  // ) =
  //   prependCalcColumn(p)(rowFun(f))

  // transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T]](
  //     inline p1: P1,
  //     p2: P2
  // )(using Functor[V]) =
  //   val schema3 = schema.rename(p1, p2)
  //   val vals    = rows.asInstanceOf[V[schema3.Values]] // to avoid iteration and map
  //   Relation[schema3.type, V](schema3)(vals)

  // transparent inline def ++[R2 <: Relation[V]](r2: R2)(using
  //     ev: r2.schema.Values =:= schema.Values
  // )(using SemigroupK[V])(using Functor[V]) =
  //   import cats.syntax.all.toSemigroupKOps
  //   // val vals = rows <+> r2.rows.map(ev)
  //   // to avoid iteration and map we cast:
  //   val vals = rows <+> r2.rows.asInstanceOf[V[schema.Values]] // to avoid iteration and map
  //   Relation(schema)(vals)

  // transparent inline def replaceRows(inline f: V[Row] => V[Row]) =
  //   Relation(schema)(f(rows))

  // transparent inline def filter(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
  //   import cats.syntax.functorFilter.given
  //   replaceRows(_.filter(predicate))
  // // TODO: try optimizing withFilter
  // transparent inline def withFilter(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
  //   import cats.syntax.functorFilter.given
  //   replaceRows(_.filter(predicate))

  // transparent inline def filterNot(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
  //   import cats.syntax.functorFilter.given
  //   replaceRows(_.filterNot(predicate))

  // /** NB: O(N + M ln M), N = rows.size, M = R2.rows.size */
  // transparent inline def --[R2 <: Relation[V]](r2: R2)(using
  //     ev: r2.schema.Values =:= schema.Values
  // )(using Foldable[V])(using FunctorFilter[V]) =
  //   import cats.syntax.all.toFoldableOps
  //   val set2 = r2.rows.asInstanceOf[V[Row]].foldLeft(Set[Row]())(_ + _)
  //   filterNot(set2.contains)

  // final def groupBy[B](f: Row => B)(using Order[B])(using Foldable[V])(using MonoidK[V])(using
  //     Applicative[V]
  // ): SortedMap[B, V[Row]] =
  //   groupMap(key = f)(identity)

  // final def groupMap[K, B](key: Row => K)(f: Row => B)(using K: Order[K])(using Foldable[V])(using
  //     SemigroupK[V]
  // )(using Applicative[V]): SortedMap[K, V[B]] =
  //   given ordering: Ordering[K] = K.toOrdering
  //   import cats.syntax.all.toFoldableOps
  //   import cats.syntax.all.toSemigroupKOps
  //   val app = Applicative[V]
  //   rows.foldLeft(SortedMap.empty[K, V[B]])((m, elem) =>
  //     val k = key(elem)
  //     m.get(k) match {
  //       case Some(b) => m.updated(key = k, value = b <+> app.pure(f(elem)))
  //       case None    => m + (k -> app.pure(f(elem)))
  //     }
  //   )
  // final def groupMapReduce[K, B](key: Row => K)(f: Row => B)(using K: Order[K], S: Semigroup[B])(
  //     using Foldable[V]
  // ): SortedMap[K, B] =
  //   groupMapReduceWith(key)(f)(S.combine)

  // final def groupMapReduceWith[K, B](key: Row => K)(f: Row => B)(combine: (B, B) => B)(using
  //     K: Order[K]
  // )(using Foldable[V]): SortedMap[K, B] =
  //   given ordering: Ordering[K] = K.toOrdering
  //   import cats.syntax.all.toFoldableOps

  //   rows.foldLeft(SortedMap.empty[K, B])((m, elem) =>
  //     val k = key(elem)

  //     m.get(k) match {
  //       case Some(b) => m.updated(key = k, value = combine(b, f(elem)))
  //       case None    => m + (k -> f(elem))
  //     }
  //   )
  // def toSemigroup[A](combineImpl: (A, A) => A): Semigroup[A] =
  //   new Semigroup[A]:
  //     def combine(a: A, b: A): A = combineImpl(a, b)

  // transparent inline def groupMapReduceS[
  //     KeySchema <: RecordSchema,
  //     AggregateSchema <: RecordSchema
  // ](
  //     keySchema: KeySchema,
  //     aggregateSchema: AggregateSchema
  //     // )(
  //     //   resultSchema: RecordSchema.Concat[keySchema.type, aggregateSchema.type],//inline schema3: RecordSchema.Concat[this.type, schema2.type]
  // )(
  //     inline k: Row => keySchema.Values,
  //     inline m: Row => aggregateSchema.Values
  // )(using Order[keySchema.Values])(using
  //     Semigroup[aggregateSchema.Values]
  // ) // to aggregate values, something like `sum`
  // (using MonoidK[V])(using Applicative[V])(using Foldable[V])
  // // : Relation[V]{
  // //   // type Schema = resultSchema.type
  // // }
  // =

  //   val resultSchema = keySchema.concat[aggregateSchema.type](
  //     aggregateSchema
  //   ) // : RecordSchema.Concat[keySchema.type, aggregateSchema.type]

  //   val grouped = groupMapReduce[keySchema.Values, aggregateSchema.Values](k)(m)

  //   convertSortedMapToRelation(keySchema, aggregateSchema)(grouped)
  //   // val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
  //   // // concat
  //   // val allVals: Iterable[resultSchema.Values] = grouped.toIterable.map(concat(_, _))
  //   // import cats.MonoidK.ops.toAllMonoidKOps
  //   // val vals = allVals.foldLeft(MonoidK[V].empty[resultSchema.Values])((b, a) => b <+> Applicative[V].pure(a))
  //   // Relation.apply(resultSchema)(vals)



// transparent inline def convertSortedMapToRelation[
//     V[_],
//     KeySchema <: RecordSchema,
//     AggregateSchema <: RecordSchema
//     // ResultSchema <: RecordSchema.Concat[KeySchema, AggregateSchema]
// ](
//     keySchema: KeySchema,
//     aggregateSchema: AggregateSchema
//     // )(
//     //   resultSchema: RecordSchema.Concat[keySchema.type, aggregateSchema.type],//inline schema3: RecordSchema.Concat[this.type, schema2.type]
// )(grouped: SortedMap[keySchema.Values, aggregateSchema.Values])(using Order[keySchema.Values])(using
//     Semigroup[aggregateSchema.Values]
// ) // to aggregate values, something like `sum`
// (using MonoidK[V])(using Applicative[V])(using Foldable[V]): Relation[V] {
//   type Schema <: RecordSchema.Concat[keySchema.type, aggregateSchema.type]
// } =
//   val resultSchema                      = keySchema.concat[aggregateSchema.type](aggregateSchema)
//   val concat                            = keySchema.concatValues(aggregateSchema)(resultSchema)
//   val allVals: Seq[resultSchema.Values] = grouped.toSeq.map(concat(_, _))
//   val vals: V[resultSchema.Values] =
//     allVals.foldLeft(MonoidK[V].empty[resultSchema.Values])((b, a) =>
//       MonoidK[V].combineK(b, Applicative[V].pure(a))
//     )
//   Relation.apply[resultSchema.type, V](resultSchema)(vals)

// transparent inline def convertSortedMapToV[
//     V[_],
//     KeySchema <: RecordSchema,
//     AggregateSchema <: RecordSchema
//     // ResultSchema <: RecordSchema.Concat[KeySchema, AggregateSchema]
// ](
//     keySchema: KeySchema,
//     aggregateSchema: AggregateSchema
// )(grouped: SortedMap[keySchema.Values, aggregateSchema.Values])(using Order[keySchema.Values])(using
//     Semigroup[aggregateSchema.Values]
// ) // to aggregate values, something like `sum`
// (using MonoidK[V])(using Applicative[V])(using Foldable[V])
// // : Relation[V]{
// //   // type Schema = resultSchema.type
// // }
// =
//   val resultSchema =
//     keySchema.concat(aggregateSchema) // : RecordSchema.Concat[keySchema.type, aggregateSchema.type]
//   val concat                            = keySchema.concatValues(aggregateSchema)(resultSchema)
//   val allVals: Seq[resultSchema.Values] = grouped.toSeq.map(concat(_, _))
//   val vals = allVals.foldLeft(MonoidK[V].empty[resultSchema.Values])((b, a) =>
//     MonoidK[V].combineK(b, Applicative[V].pure(a))
//   )
//   vals

object Relation:
  transparent inline def apply[S1 <: RecordSchema, VS1, V[_]](s1: S1)(using svt1: SchemaValueType[S1, VS1])(inline v: V[svt1.Value]) =
    new Relation[S1, VS1, V](s1)(using svt1) {
      type Schema = S1
      val rows   = v.asInstanceOf[V[VS1]]
    }

  // transparent inline def empty[S1 <: RecordSchema, V[_]](inline s1: S1)(using MonoidK[V]) =
  //   apply(s1)(MonoidK[V].empty)

  // type RelationOf[S <: RecordSchema] = [V[_]] =>> Relation[V] {
  //   type Schema = S
  // }
  // type RelationOfV[V[_]] = [S <: RecordSchema] =>> Relation[V] {
  //   type Schema = S
  // }

extension (tb: TableBuilder)
  transparent inline def relation[TSV, V[_]](
    using rsvt: SchemaValueType[tb.TableSchema, TSV]
    )(values1: V[rsvt.Value]): Relation[tb.TableSchema, TSV, V] =
    Relation(tb.tableSchema)(values1)
