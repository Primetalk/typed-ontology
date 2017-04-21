package ru.primetalk.typed.ontology.metameta

import ru.primetalk.typed.ontology.metameta

import scala.language.higherKinds

/**
  * This trait defines framework for various data record representation mechanisms.
  *
  */
trait RecordRepresentation {

  val meta: Meta
  /** Type of a particular form of record. */
  type RecordImpl[A]

  /** Type mapping from [[Record]] to implementation. */
  implicit def mapRecordToImpl[A]: metameta.TypeMapping[Record[A], RecordImpl[A]] = typeMapping

  /** Part of record is just a single property with it's value. */
  type PropertyValue[PropertyIdImpl[-_,_], A, B, D]

  implicit val recordSupport: RecordTypeClass[RecordImpl, meta.PropertyIdImpl]
}
