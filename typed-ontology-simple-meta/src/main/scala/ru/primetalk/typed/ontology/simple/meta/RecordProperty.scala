package ru.primetalk.typed.ontology.simple.meta

import scala.reflect.ClassTag
import ru.primetalk.typed.ontology.metameta.Record
import ru.primetalk.typed.ontology.utils.objectName

sealed trait RecordProperty0:
  // Record type
  type R
  // Property type
  type P

  val name: String

/** Type of record property identifier. */
sealed trait RecordProperty[A] extends RecordProperty0:
  type R = A
/**
  * Metainformation about property.
  * Contains unique name (within the type) and type of the value.
  * Might contain other metainformation about property, like Schema.
  */
abstract class SimplePropertyId[A,B](name1: String, val tpe: ClassTag[B]) extends RecordProperty[A]:
  type P = B

  val name: String = name1
  
  def tpeSimpleName = 
    tpe.runtimeClass.getSimpleName

  override def toString: String = 
    s"$name: $tpeSimpleName"

object RecordProperty0:
  type PropertyValueType[A] = A match
    case SimplePropertyId[_, p] => p
    case _ => Nothing

trait PropertiesBuilder extends RecordSchemaBuilderBase:
  transparent inline def property[T: ClassTag](inline name: String) = 
    new SimplePropertyId[RecordType, T](name, summon){}
  /** Convenient mechanism to create a self-typed property.*/
  abstract class column[T: ClassTag] extends SimplePropertyId[RecordType, T]("", summon):
    override val name = objectName(this)
