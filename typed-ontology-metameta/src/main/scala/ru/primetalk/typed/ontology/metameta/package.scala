package ru.primetalk.typed.ontology.metameta

import scala.language.{higherKinds, implicitConversions}
import ru.primetalk.typed.ontology.metameta.RttiProvider

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

abstract final class OntologyEnum[A] extends OntologyType

trait PropertyIdTypeClass[PropertyId[-_,_ <: OntologyType]]:
  def name[A, B <: OntologyType](p: PropertyId[A,B]): String

given scalarRtti[T](using tprov: RttiProvider[T]): RttiProvider[Scalar[T]] = new RttiProvider[Scalar[T]]:
  def rtti = tprov.rtti

given metaSeqRtti[T <: OntologyType](using tprov: RttiProvider[T]): RttiProvider[MetaSeq[T]] = new RttiProvider[MetaSeq[T]]:
  def rtti = RuntimeTypeInformation.SeqType(tprov.rtti)
