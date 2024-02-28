package ru.primetalk.typed.ontology.simple.meta

import scala.reflect.ClassTag
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import ru.primetalk.typed.ontology.utils.objectName

sealed trait RecordProperty0:
  // Record phantom type
  type R
  // Property schema
  type P <: SchemaLike

  val name: String

/** Type of record property identifier. */
sealed trait RecordProperty[A] extends RecordProperty0:
  type R = A

/** Metainformation about property with a known schema. */    
abstract class SchemaBasedPropertyId[A, S <: SchemaLike](name1: String, val schema: S)
    extends RecordProperty[A]:
  type P = S

  val name: String = name1

  def tpeSimpleName =
    schema.toString()

  override def toString: String =
    s"$name: $tpeSimpleName"

/** Metainformation about property. Contains unique name (within the type) and type of the value.
  * Might contain other metainformation about property, like Schema.
  */
abstract class SimplePropertyId[A, B: ClassTag](name1: String) extends SchemaBasedPropertyId[A, ScalarSchema1[B]](name1, summon[ScalarSchema1[B]])

object RecordProperty0:
  type PropertyValueType[A] = A match
    case SimplePropertyId[_, p] => p
    case _                      => Nothing

trait PropertiesBuilder extends RecordSchemaBuilderBase:
  transparent inline def property[T: ClassTag](inline name: String) =
    new SimplePropertyId[RecordType, T](name) {}

  /** Convenient mechanism to create a self-typed property. */
  abstract class column[T: ClassTag] extends SimplePropertyId[RecordType, T](""):
    override val name = objectName(this)
