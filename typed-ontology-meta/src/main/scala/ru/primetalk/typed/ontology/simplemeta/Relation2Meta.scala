package ru.primetalk.typed.ontology.simplemeta

// import cats.Map
import cats.Functor
import cats.FlatMap

abstract class Relation2Meta[V[_]]:
  self =>
  type Schema <: RecordSchema
  val schema: Schema
  val rows: V[schema.Values]

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
    // val schema3 = schema.concat[schema.type, r2.schema.type](r2.schema)//[r2.schema.type]
    val f: (r1.schema.Values, schema.Values) => schema3.Values = schema.prependValues(r1.schema)(schema3)
    // schema3.toString
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

object Relation2Meta:
  transparent inline def apply[S1 <: RecordSchema, V[_]](inline s1: S1)(inline v: V[s1.Values]) =
    new Relation2Meta[V] {
      type Schema = s1.type
      val schema = s1
      val rows = v
    }
  transparent inline def crossProduct[V[_], R1 <: Relation2Meta[V], R2 <: Relation2Meta[V]](inline r1: R2, inline r2: R2)(using FlatMap[V]) =
    r2.crossProductFrom(r1)
