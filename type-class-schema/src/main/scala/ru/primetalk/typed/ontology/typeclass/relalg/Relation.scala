package ru.primetalk.typed.ontology.typeclass.relalg

import scala.language.experimental.namedTuples
import ru.primetalk.typed.ontology.typeclass.schema.Projector
import ru.primetalk.typed.ontology.typeclass.schema.ValueWithSchema
import cats.FlatMap
import cats.Functor
import cats.FunctorFilter

/** Отношение содержит как схему, так и элементы в коллекции Coll.
 * 
 * "Коллекцией", впрочем, может выступать и что-то другое - Future, Stream и т.п. 
 * Лишь бы были определены соответствующие type-классы - Functor, FlatMap, etc.
 */
type Relation[S <: Tuple, Value, Coll[_]] = Coll[ValueWithSchema[S, Value]]

extension [S <: Tuple, Value <: Tuple, Coll[_]](r: Relation[S, Value, Coll])
  /**
    * Projects original relation to the target schema.
    * 
    * @tparam Dest - target schema
    */
  def projection[Dest <: Tuple, DestV <: Tuple](using t: Projector[S, Value, Dest, DestV], f: Functor[Coll]): Relation[Dest, DestV, Coll] = 
    f.map(r)(t)

  def cartesianProduct[Other <: Tuple, OtherV <: Tuple](other: Relation[Other, OtherV, Coll])(using f: FlatMap[Coll]): Relation[Tuple.Concat[S, Other], Tuple.Concat[Value, OtherV], Coll] = 
    f.flatMap(r)(row1 => f.map(other)((row2: ValueWithSchema[Other, OtherV]) => 
      (row1.value ++ row2.value): ValueWithSchema[Tuple.Concat[S, Other], Tuple.Concat[Value, OtherV]]
    ))

  def innerJoin[Other <: Tuple, OtherV <: Tuple](other: Relation[Other, OtherV, Coll], 
    p: ValueWithSchema[Tuple.Concat[S, Other], Tuple.Concat[Value, OtherV]] => Boolean)(using f: FlatMap[Coll], fi: cats.FunctorFilter[Coll]): Relation[Tuple.Concat[S, Other], Tuple.Concat[Value, OtherV], Coll] = 
    fi.mapFilter(cartesianProduct(other))(v => if p(v) then Some(v) else None)
