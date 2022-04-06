package ru.primetalk.typed.ontology.nested.meta

import scala.reflect.ClassTag
import ru.primetalk.typed.ontology.metameta.{OntologyType, Record, Scalar, MetaSeq, RuntimeTypeInformation, RttiProvider, OntologyEnum}
import ru.primetalk.typed.ontology.utils.objectName

import RuntimeTypeInformation.{NamedType, EntityType}

sealed trait RecordProperty0:
  // Record type
  type R
  // Property type
  type P <: OntologyType

  val name: String

/** Type of record property identifier. */
sealed trait RecordProperty[A] extends RecordProperty0:
  type R = A
/**
  * Metainformation about property.
  * Contains unique name (within the type) and type of the value.
  * Might contain other metainformation about property, like Schema.
  */
abstract class AttributeId[A, B <: OntologyType](name1: String, val tpe: RttiProvider[B]) extends RecordProperty[A]:
  type P = B

  val name: String = name1
  
  def tpeSimpleName = 
    tpe.rtti.toString

  override def toString: String = 
    s"$name: $tpeSimpleName"

object RecordProperty0:
  type PropertyValueType[A] = A match
    case AttributeId[_, Record[p]] => Record[p]
    case AttributeId[_, Scalar[p]] => p
    case _ => Nothing

trait PropertiesBuilder extends RecordSchemaBuilderBase:

  transparent inline def property[T](inline name: String)(using RttiProvider[Scalar[T]])  = 
    new AttributeId[RecordType, Scalar[T]](name, summon){}
  /** Convenient mechanism to create a self-typed property.*/
  abstract class column[T <: OntologyType](using RttiProvider[T]) extends AttributeId[RecordType, T]("", summon):
    override val name = objectName(this)

  /** Convenient mechanism to create a self-typed property.*/
  abstract class scalarColumn[T](using RttiProvider[Scalar[T]]) extends column[Scalar[T]]

  abstract class nestedRecord[T](using RttiProvider[Record[T]]) extends column[Record[T]]

  abstract class enumColumn[T](using RttiProvider[OntologyEnum[T]]) extends column[OntologyEnum[T]]

  abstract class seq[T<: OntologyType](using RttiProvider[MetaSeq[T]])  extends column[MetaSeq[T]]

  abstract class seqOfScalar[T](using RttiProvider[MetaSeq[Scalar[T]]]) extends seq[Scalar[T]]

  abstract class seqOfRecord[T](using RttiProvider[MetaSeq[Record[T]]]) extends seq[Record[T]]

  abstract class seqOfEnum[T](using RttiProvider[MetaSeq[OntologyEnum[T]]]) extends seq[OntologyEnum[T]]

  def defineEntityType(name: String, columns: AttributeId[RecordType, _]*): RttiProvider[Record[RecordType]] = new RttiProvider[Record[RecordType]]:
    def rtti = NamedType(name, EntityType(columns.map(a => (a.name, a.tpe.rtti)).toMap))

  def defineEntityType(columns: AttributeId[RecordType, _]*): RttiProvider[Record[RecordType]] =
    defineEntityType(objectName(this), columns:_*)
