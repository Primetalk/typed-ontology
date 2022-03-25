package ru.primetalk.typed.ontology.nested.meta

import scala.reflect.ClassTag
import ru.primetalk.typed.ontology.OntologyType
import ru.primetalk.typed.ontology.Record
import ru.primetalk.typed.ontology.Scalar

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
abstract class AttributeId[A, B <: OntologyType](name1: String, val tpe: ClassTag[B]) extends RecordProperty[A]:
  type P = B

  val name: String = name1
  
  def tpeSimpleName = 
    tpe.runtimeClass.getSimpleName

  override def toString: String = 
    s"$name: $tpeSimpleName"

object RecordProperty0:
  type PropertyValueType[A] = A match
    case AttributeId[_, Record[p]] => Record[p]
    case AttributeId[_, Scalar[p]] => p
    case _ => Nothing

trait PropertiesBuilder extends RecordSchemaBuilderBase:
  transparent inline def property[T: ClassTag](inline name: String) = 
    new AttributeId[RecordType, Scalar[T]](name, summon){}
  /** Convenient mechanism to create a self-typed property.*/
  abstract class column[T: ClassTag] extends AttributeId[RecordType, Scalar[T]]("", summon):
    override val name = this.getClass.getSimpleName.replace("$", "")

  abstract class nestedRecord[T: ClassTag] extends AttributeId[RecordType, Record[T]]("", summon):
    override val name = this.getClass.getSimpleName.replace("$", "")
