package ru.primetalk.typed.ontology.simple.tmap

import scala.collection.SortedMap
import ru.primetalk.typed.ontology.simple.meta.RecordProperty
import ru.primetalk.typed.ontology.simple.meta.RecordProperty0

/** 
 * Typed map allows to obtain the value of property and update it when needed.
 * This is a simple one-level implementation of typed maps. Does not naturally support nested objects.
 */
trait TypedMap[M[_]] {
  def apply[R, P  <: RecordProperty[R]](m: M[R])(p: P): Option[RecordProperty0.PropertyValueType[p.type]]
  def updated[R, P <: RecordProperty[R]](m: M[R])(p: P, v: Option[RecordProperty0.PropertyValueType[p.type]]): M[R]
}

extension [M[_], R](tm: M[R])(using TypedMap[M])
  def apply[P  <: RecordProperty[R]](p: P): Option[RecordProperty0.PropertyValueType[p.type]] =
    summon[TypedMap[M]].apply(tm)(p)
  def updated[P <: RecordProperty[R]](p: P, v: Option[RecordProperty0.PropertyValueType[p.type]]): M[R] =
    summon[TypedMap[M]].updated(tm)(p, v)

opaque type SimpleTypedMap[R] = Map[String, Any]

object SimpleTypedMap:
  given TypedMap[SimpleTypedMap] with
    def apply[R, P  <: RecordProperty[R]](m: SimpleTypedMap[R])(p: P): Option[RecordProperty0.PropertyValueType[p.type]] =
      m.get(p.name).asInstanceOf[Option[RecordProperty0.PropertyValueType[p.type]]]
    def updated[R, P <: RecordProperty[R]](m: SimpleTypedMap[R])(p: P, v: Option[RecordProperty0.PropertyValueType[p.type]]): SimpleTypedMap[R] =
      v match
        case Some(v) =>
          m + (p.name -> v)
        case None =>
          m -- Seq(p.name)
