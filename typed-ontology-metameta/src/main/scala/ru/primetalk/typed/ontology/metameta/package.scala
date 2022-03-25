package ru.primetalk.typed.ontology

import scala.language.{higherKinds, implicitConversions}

/**
  * Meta-meta package contains instruments to define meta level of ontology.
  *
  * For instance here we define type classes that allows us to use arbitrary 
  * types as property identifiers.
  */
sealed trait OntologyType
/** Phantom type that represents a record. */
abstract final class Record[+A] extends OntologyType
/** A type that represents a simple Scala type that is supported. */
abstract final class Scalar[A] extends OntologyType

/** Phantom type that represents a collection of elements of type A. */
abstract final class MetaSeq[+A <: OntologyType] extends OntologyType

trait PropertyIdTypeClass[PropertyId[-_,_ <: OntologyType]]:
  def name[A, B <: OntologyType](p: PropertyId[A,B]): String
