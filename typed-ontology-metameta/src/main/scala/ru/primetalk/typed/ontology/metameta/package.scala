package ru.primetalk.typed.ontology

import scala.language.{higherKinds, implicitConversions}

/**
  * Meta-meta package contains instruments to define meta level of ontology.
  *
  * For instance here we define type classes that allows us to use arbitrary types as property identifiers.
  */
sealed trait AttributeType
/** Phantom type that represents a record. */
abstract final class Record[+A] extends AttributeType
/** A type that represents a simple Scala type that is supported. */
abstract final class Scalar[A] extends AttributeType

/** Phantom type that represents a collection of elements of type A. */
abstract final class MetaSeq[+A <: AttributeType] extends AttributeType

trait PropertyIdTypeClass[PropertyId[-_,_]]:
  def name[A,B](p: PropertyId[A,B]): String
