package ru.primetalk.typed.ontology.simple.relalg

// import cats.Map
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
import ru.primetalk.typed.ontology.simple.meta.TableBuilder

/**
 * Relation is a pair of schema and a collection of instances of that schema.
 * V - is the collection type (List, Stream[...]).
 */
abstract class Relation[V[_]] extends PredicateClassicDsl:
  self =>
  
  type Schema <: RecordSchema
  val schema: Schema
  type Row = schema.Values
  val rows: V[schema.Values]
  type Self = Relation[V] {
    type Schema = self.Schema
    type Row = self.Row
  }
  def show(using Foldable[V]) =
    import cats.Foldable.ops.toAllFoldableOps

    schema.toString + 
      "\n-----\n" + 
      rows
        .foldLeft(List[String]())((lst, row) => row.toString :: lst)
        .reverse
        .mkString("\n")

  transparent inline def projection[S2 <: RecordSchema](inline s2: S2)(using Functor[V]) =
    import cats.Functor.ops.toAllFunctorOps
    val f = s2.projectorFrom(schema)
    val vals = rows.map(f)
    Relation(s2)(vals)

  transparent inline def crossProductFrom[R1 <: Relation[V]](inline r1: R1)(using FlatMap[V]): Relation[V] =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = r1.schema.appendOtherSchema(schema)
    val f: (r1.schema.Values, schema.Values) => schema3.Values = r1.schema.appendValues(schema)(schema3)
    val vals = 
      for
        row1 <- r1.rows
        row2 <- this.rows
      yield
        f(row1, row2)
    Relation(schema3)(vals)

  transparent inline def crossProduct[R2 <: Relation[V]](inline r2: R2)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = schema.appendOtherSchema(r2.schema)
    val f: (schema.Values, r2.schema.Values) => schema3.Values = schema.appendValues(r2.schema)(schema3)
    val vals = 
      for
        row1 <- this.rows
        row2 <- r2.rows
      yield
        f(row1, row2)
    Relation[schema3.type, V](schema3)(vals)

  transparent inline def join[FK <: ForeignKeyId0, R2 <: Relation[V]](inline fk: FK)(inline r2: R2)(using FlatMap[V])(using FunctorFilter[V]) = 
    import cats.FlatMap.ops.toAllFlatMapOps
    import cats.FunctorFilter.ops.toAllFunctorFilterOps
    val schema3 = schema.appendOtherSchema(r2.schema)
    val f: (schema.Values, r2.schema.Values) => schema3.Values = schema.appendValues(r2.schema)(schema3)
    val pred: schema3.Values => Boolean = schema3.fkPredicate(fk)
    val vals = 
      for
        row1 <- this.rows
        row2 <- r2.rows
        row3 = f(row1, row2)
        // if pred(row3)
      yield
        row3
    val filtered = vals.filter(pred)
    Relation[schema3.type, V](schema3)(filtered)

  transparent inline def prependCalcColumn[P <: RecordProperty0](inline p: P)(inline f: Row => p.P)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = p #: schema
    val vals = rows.map(row => (f(row) *: row).asInstanceOf[schema3.Values])
    Relation(schema3)(vals)

  transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T]](inline p1: P1, p2: P2)(using Functor[V]) =
    val schema3 = schema.rename(p1, p2)
    val vals = rows.asInstanceOf[V[schema3.Values]] // to avoid iteration and map
    Relation[schema3.type, V](schema3)(vals)

  transparent inline def ++[R2 <: Relation[V]](inline r2: R2)(using ev: r2.schema.Values =:= schema.Values)(using SemigroupK[V])(using Functor[V]) =
    import cats.syntax.all.toSemigroupKOps
    // val vals = rows <+> r2.rows.map(ev)
    // to avoid iteration and map we cast:
    val vals = rows <+> r2.rows.asInstanceOf[V[schema.Values]] // to avoid iteration and map
    Relation(schema)(vals)

  transparent inline def replaceRows(inline f: V[Row] => V[Row]) =
    Relation(schema)(f(rows))

  transparent inline def filter(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
    import cats.FunctorFilter.ops.toAllFunctorFilterOps
    replaceRows(_.filter(predicate))
  // TODO: try optimizing withFilter
  transparent inline def withFilter(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
    import cats.FunctorFilter.ops.toAllFunctorFilterOps
    replaceRows(_.filter(predicate))

  transparent inline def filterNot(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
    import cats.FunctorFilter.ops.toAllFunctorFilterOps
    replaceRows(_.filterNot(predicate))

  /** NB: O(N + M ln M), N = rows.size, M = R2.rows.size */
  transparent inline def --[R2 <: Relation[V]](inline r2: R2)(using ev: r2.schema.Values =:= schema.Values)(using Foldable[V])(using FunctorFilter[V]) =
    import cats.syntax.all.toFoldableOps
    val set2 = r2.rows.asInstanceOf[V[Row]].foldLeft(Set[Row]())(_ + _)
    filterNot(set2.contains)

  final def groupBy[B](f: Row => B)(using Order[B])(using Foldable[V])(using MonoidK[V])(using Applicative[V]): SortedMap[B, V[Row]] =
    groupMap(key = f)(identity)
 
  final def groupMap[K, B](key: Row => K)(f: Row => B)(using K: Order[K])(using Foldable[V])(using SemigroupK[V])(using Applicative[V]): SortedMap[K, V[B]] =
    given ordering: Ordering[K] = K.toOrdering
    import cats.syntax.all.toFoldableOps
    import cats.syntax.all.toSemigroupKOps
    val app = Applicative[V]
    rows.foldLeft(SortedMap.empty[K, V[B]])( (m, elem) => 
      val k = key(elem)
      m.get(k) match {
        case Some(b) => m.updated(key = k, value = b <+> app.pure(f(elem)))
        case None    => m + (k -> app.pure(f(elem)))
      }
    )
  final def groupMapReduce[K, B](key: Row ⇒ K)(f: Row ⇒ B)(using K: Order[K], S: Semigroup[B])(using Foldable[V]): SortedMap[K, B] = 
    groupMapReduceWith(key)(f)(S.combine)
    
  final def groupMapReduceWith[K, B](key: Row => K)(f: Row => B)(combine: (B, B) => B)(using K: Order[K])(using Foldable[V]): SortedMap[K, B] = 
    given ordering: Ordering[K] = K.toOrdering
    import cats.syntax.all.toFoldableOps

    rows.foldLeft(SortedMap.empty[K, B])( (m, elem) => 
      val k = key(elem)

      m.get(k) match {
        case Some(b) => m.updated(key = k, value = combine(b, f(elem)))
        case None    => m + (k -> f(elem))
      }
      
    )
  def toSemigroup[A](combineImpl: (A, A) => A): Semigroup[A] = 
    new Semigroup[A]:
      def combine(a: A, b: A): A = combineImpl(a,b)


  transparent inline def groupMapReduceS[
    KeySchema <: RecordSchema,
    AggregateSchema <: RecordSchema
    ](
    inline keySchema: KeySchema,
    inline aggregateSchema: AggregateSchema
    )(
      inline resultSchema: RecordSchema.Concat[keySchema.type, aggregateSchema.type],//inline schema3: RecordSchema.Concat[this.type, schema2.type]
    )(
      inline k: Row => keySchema.Values,
      inline m: Row => aggregateSchema.Values
    )(using Order[keySchema.Values])
    (using Semigroup[aggregateSchema.Values])// to aggregate values, something like `sum`
    (using MonoidK[V])
    (using Applicative[V])
    (using Foldable[V])
    // : Relation[V]{
    //   // type Schema = resultSchema.type
    // }
     = 
      // (
      // val resultSchema = keySchema.concat(aggregateSchema)// : RecordSchema.Concat[keySchema.type, aggregateSchema.type]
      // )
      val grouped = groupMapReduce[keySchema.Values, aggregateSchema.Values](k)(m)

      convertSortedMapToRelation(keySchema, aggregateSchema)(resultSchema)(grouped)
      // val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
      // // concat
      // val allVals: Iterable[resultSchema.Values] = grouped.toIterable.map(concat(_, _))
      // import cats.MonoidK.ops.toAllMonoidKOps
      // val vals = allVals.foldLeft(MonoidK[V].empty[resultSchema.Values])((b, a) => b <+> Applicative[V].pure(a))
      // Relation.apply(resultSchema)(vals)
transparent inline def convertSortedMapToRelation[
  V[_],
  KeySchema <: RecordSchema,
  AggregateSchema <: RecordSchema,
  // ResultSchema <: RecordSchema.Concat[KeySchema, AggregateSchema]
  ](
  inline keySchema: KeySchema,
  inline aggregateSchema: AggregateSchema,
  )(
    inline resultSchema: RecordSchema.Concat[keySchema.type, aggregateSchema.type],//inline schema3: RecordSchema.Concat[this.type, schema2.type]
  )(
    grouped: SortedMap[keySchema.Values, aggregateSchema.Values])
  (using Order[keySchema.Values])
  (using Semigroup[aggregateSchema.Values])// to aggregate values, something like `sum`
  (using MonoidK[V])
  (using Applicative[V])
  (using Foldable[V])
  : Relation[V]{
    type Schema = resultSchema.type
  }
    = 
    val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
    val allVals: Iterable[resultSchema.Values] = grouped.toIterable.map(concat(_, _))
    val vals: V[resultSchema.Values] = allVals.foldLeft(MonoidK[V].empty[resultSchema.Values])(
      (b, a) => 
      MonoidK[V].combineK(b, Applicative[V].pure(a)))
    Relation.apply[RecordSchema.Concat[keySchema.type, aggregateSchema.type], V](resultSchema)(vals)
    
transparent inline def convertSortedMapToV[
  V[_],
  KeySchema <: RecordSchema,
  AggregateSchema <: RecordSchema
  // ResultSchema <: RecordSchema.Concat[KeySchema, AggregateSchema]
  ](
  inline keySchema: KeySchema,
  inline aggregateSchema: AggregateSchema
  )
  (grouped: SortedMap[keySchema.Values, aggregateSchema.Values])
  (using Order[keySchema.Values])
  (using Semigroup[aggregateSchema.Values])// to aggregate values, something like `sum`
  (using MonoidK[V])
  (using Applicative[V])
  (using Foldable[V])
  // : Relation[V]{
  //   // type Schema = resultSchema.type
  // }
    = 
    val resultSchema = keySchema.concat(aggregateSchema)// : RecordSchema.Concat[keySchema.type, aggregateSchema.type]
    val concat = keySchema.concatValues(aggregateSchema)(resultSchema)
    val allVals: Iterable[resultSchema.Values] = grouped.toIterable.map(concat(_, _))
    val vals = allVals.foldLeft(MonoidK[V].empty[resultSchema.Values])(
      (b, a) => 
      MonoidK[V].combineK(b, Applicative[V].pure(a)))
    vals

object Relation:
  transparent inline def apply[S1 <: RecordSchema, V[_]](inline s1: S1)(inline v: V[s1.Values]) =
    new Relation[V] {
      type Schema = s1.type
      val schema = s1
      val rows = v
    }

  transparent inline def empty[S1 <: RecordSchema, V[_]](inline s1: S1)(using MonoidK[V]) =
    apply(s1)(MonoidK[V].empty)

  type RelationOf[S <: RecordSchema] = [V[_]] =>> Relation[V] {
      type Schema = S
  }
  type RelationOfV[V[_]] = [S <: RecordSchema] =>> Relation[V] {
      type Schema = S
  }

extension (tb: TableBuilder)
  transparent inline def relation[V[_]](inline values1: V[tb.Row]): Relation[V] = 
    Relation(tb.tableSchema)(values1)
