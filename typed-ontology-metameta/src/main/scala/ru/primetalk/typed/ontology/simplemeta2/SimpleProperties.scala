package ru.primetalk.typed.ontology.simplemeta2

import scala.reflect.ClassTag
import ru.primetalk.typed.ontology.metameta.OntologyType.Record

sealed trait RecordProperty0:
  type R
  type RR = Record[R]
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
abstract class SimplePropertyId[A,B](val name: String, val tpe: ClassTag[B]) extends RecordProperty[A]:
  type P = B

  def tpeSimpleName = 
    tpe.runtimeClass.getSimpleName

  override def toString: String = 
    s"$name: $tpeSimpleName"

object RecordProperty0:
  type PropertyValueType[A] = A match
    case SimplePropertyId[_, p] => p
    case _ => Nothing

trait PropertiesBuilder extends RecordSchemaBuilderBase:
  transparent inline def property[T: ClassTag](name: String) = 
    new SimplePropertyId[RecordType, T](name, summon){}
