package ru.primetalk.typed.ontology.typeclass.schema

import scala.compiletime.summonAll

/** 
 * This type class provides evidence that T represents a schema made of columns.
 * 
 * @tparam T is a Tuple that represents a RecordSchema. 
 */
trait RecordSchema[T <: Tuple]:
  def columns: T

object RecordSchema:
  /** Infers record schema instance from types. */
  inline def infer[T <: Tuple: RecordSchema]: T =
    summon[RecordSchema[T]].columns

  private object EmptyRecordSchema extends RecordSchema[EmptyTuple]:
    val columns: EmptyTuple = EmptyTuple

  given emptySchema: RecordSchema[EmptyTuple] =
    EmptyRecordSchema

  
  given nonEmptySchema[H: Column: ValueOf, T <: Tuple: RecordSchema]: RecordSchema[H *: T] =
    new:
      def columns: H *: T = valueOf[H] *: summon[RecordSchema[T]].columns
