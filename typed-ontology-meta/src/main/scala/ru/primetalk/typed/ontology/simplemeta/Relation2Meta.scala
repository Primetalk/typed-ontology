package ru.primetalk.typed.ontology.simplemeta

// import cats.Map
import cats.Functor
import cats.FlatMap
import cats.Foldable
import cats.Traverse
import cats.SemigroupK
import cats.MonoidK
import cats.FunctorFilter

/*
- Cartesian product, 
- join, 
- projection and 
- selection are implemented.

A generic representation might be a 
- Map[String, Any], 
- a tuple with appropriate values, 
- a tuple of Options, 
- a tuple of Eithers, … 
- conversion becomes error free, straight-forward and fully automated.  
This opens a path to implement 
Cartesian products, 
projections, and other operations of relational algebra. 

*/

// TODO: specific relational algebra operations:
//       DONE: projection Π
//       DONE: rename (ρ)
//       DONE: cross product, 
//       join,
//       Natural join (⋈)
//         on foreign key
// DONE: collection operations:
//       DONE: set union,
//       DONE: set difference? - via replaceRows
//       DONE: selection σ (filtering)
// DONE: calculate columns
// TODO: groupBy + aggregate (groupMapReduce?)

abstract class Relation2Meta[V[_]]:
  self =>
  type Schema <: RecordSchema
  val schema: Schema
  type Row = schema.Values
  val rows: V[schema.Values]

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
    val vals = self.rows.map(f)
    new Relation2Meta[V] {
      type Schema = s2.type
      val schema = s2
      val rows = vals
    }

  transparent inline def crossProductFrom[R1 <: Relation2Meta[V]](inline r1: R1)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = schema.prependOtherSchema(r1.schema)
    val f: (r1.schema.Values, schema.Values) => schema3.Values = schema.prependValues(r1.schema)(schema3)
    val vals = 
      for
        row1 <- r1.rows
        row2 <- this.rows
      yield
        f(row1, row2)
    Relation2Meta[schema3.type, V](schema3)(vals)

  transparent inline def crossProduct[R2 <: Relation2Meta[V]](inline r2: R2)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = schema.appendOtherSchema(r2.schema)
    val f: (schema.Values, r2.schema.Values) => schema3.Values = schema.appendValues(r2.schema)(schema3)
    val vals = 
      for
        row1 <- this.rows
        row2 <- r2.rows
      yield
        f(row1, row2)
    Relation2Meta[schema3.type, V](schema3)(vals)

  transparent inline def prependCalcColumn[P <: RecordProperty0](inline p: P)(inline f: Row => p.P)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = p #: schema
    val vals = rows.map(row => (f(row) *: row).asInstanceOf[schema3.Values])
    Relation2Meta(schema3)(vals)

  transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T]](inline p1: P1, p2: P2)(using Functor[V]) =
    val schema3 = schema.rename(p1, p2)
    val vals = rows.asInstanceOf[V[schema3.Values]] // to avoid iteration and map
    Relation2Meta[schema3.type, V](schema3)(vals)

  transparent inline def ++[R2 <: Relation2Meta[V]](inline r2: R2)(using ev: r2.schema.Values =:= schema.Values)(using SemigroupK[V])(using Functor[V]) =
    import cats.syntax.all.toSemigroupKOps
    // val vals = rows <+> r2.rows.map(ev)
    // to avoid iteration and map we cast:
    val vals = rows <+> r2.rows.asInstanceOf[V[schema.Values]] // to avoid iteration and map
    Relation2Meta(schema)(vals)

  transparent inline def replaceRows(inline f: V[Row] => V[Row]) =
    Relation2Meta(schema)(f(rows))

  transparent inline def filter(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
    import cats.FunctorFilter.ops.toAllFunctorFilterOps
    replaceRows(_.filter(predicate))

  transparent inline def filterNot(inline predicate: Row => Boolean)(using FunctorFilter[V]) =
    import cats.FunctorFilter.ops.toAllFunctorFilterOps
    replaceRows(_.filterNot(predicate))

  transparent inline def --[R2 <: Relation2Meta[V]](inline r2: R2)(using ev: r2.schema.Values =:= schema.Values)(using Foldable[V])(using FunctorFilter[V]) =
    import cats.syntax.all.toFoldableOps
    val set2 = r2.rows.asInstanceOf[V[Row]].foldLeft(Set[Row]())(_ + _)
    filterNot(set2.contains)

object Relation2Meta:
  transparent inline def apply[S1 <: RecordSchema, V[_]](inline s1: S1)(inline v: V[s1.Values]) =
    new Relation2Meta[V] {
      type Schema = s1.type
      val schema = s1
      val rows = v
    }

  transparent inline def empty[S1 <: RecordSchema, V[_]](inline s1: S1)(using MonoidK[V]) =
    apply(s1)(MonoidK[V].empty)
