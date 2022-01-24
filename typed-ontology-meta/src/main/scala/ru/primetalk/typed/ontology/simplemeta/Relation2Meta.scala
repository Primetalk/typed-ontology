package ru.primetalk.typed.ontology.simplemeta

// import cats.Map
import cats.Functor
import cats.FlatMap
import cats.Foldable

// TODO: specific relational algebra operations:
//       projection Π
//       rename (ρ)
//       cross product, join, Natural join (⋈) on foreign key
// TODO: collection operations:
//       set union, set difference?
//       selection σ (filtering)
// DONE: calculate columns
// TODO: aggregate

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
    val v = self.rows.map(f)
    new Relation2Meta[V] {
      type Schema = s2.type
      val schema = s2
      val rows = v
    }

  transparent inline def crossProductFrom[R1 <: Relation2Meta[V]](inline r1: R1)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = schema.prependOtherSchema(r1.schema)
    val f: (r1.schema.Values, schema.Values) => schema3.Values = schema.prependValues(r1.schema)(schema3)
    val v = 
      for
        row1 <- r1.rows
        row2 <- this.rows
      yield
        f(row1, row2)
    new Relation2Meta[V] {
      type Schema = schema3.type
      val schema = schema3
      val rows = v
    }
  transparent inline def crossProduct[R2 <: Relation2Meta[V]](inline r2: R2)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = schema.appendOtherSchema(r2.schema)
    val f: (schema.Values, r2.schema.Values) => schema3.Values = schema.appendValues(r2.schema)(schema3)
    val v = 
      for
        row1 <- this.rows
        row2 <- r2.rows
      yield
        f(row1, row2)
    new Relation2Meta[V] {
      type Schema = schema3.type
      val schema = schema3
      val rows = v
    }

  transparent inline def prependCalcColumn[P <: RecordProperty0](inline p: P)(inline f: Row => p.P)(using FlatMap[V]) =
    import cats.FlatMap.ops.toAllFlatMapOps
    val schema3 = p #: schema
    val vals = rows.map(row => (f(row) *: row).asInstanceOf[schema3.Values])
    new Relation2Meta[V] {
      type Schema = schema3.type
      val schema = schema3
      val rows = vals
    }
  transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T]](inline p1: P1, p2: P2)(using Functor[V]) =
    val schema3 = schema.rename(p1, p2)
    val vals = rows.asInstanceOf[V[schema3.Values]] // to avoid iteration and map
    new Relation2Meta[V] {
      type Schema = schema3.type
      val schema = schema3
      val rows = vals
    }

object Relation2Meta:
  transparent inline def apply[S1 <: RecordSchema, V[_]](inline s1: S1)(inline v: V[s1.Values]) =
    new Relation2Meta[V] {
      type Schema = s1.type
      val schema = s1
      val rows = v
    }
  transparent inline def crossProduct[V[_], R1 <: Relation2Meta[V], R2 <: Relation2Meta[V]](inline r1: R2, inline r2: R2)(using FlatMap[V]) =
    r2.crossProductFrom(r1)
