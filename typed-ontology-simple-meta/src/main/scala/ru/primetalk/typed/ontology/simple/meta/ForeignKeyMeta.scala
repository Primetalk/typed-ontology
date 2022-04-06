package ru.primetalk.typed.ontology.simple.meta

import ru.primetalk.typed.ontology.metameta.Record

sealed trait ForeignKeyId0:
  type Left <: RecordProperty0
  type Right <: RecordProperty0
  val left: Left
  val right: Right

abstract class ForeignKeyId[R1, R2, T, P1 <: SimplePropertyId[R1, T], P2 <: SimplePropertyId[R2, T]](val prop1: P1, val prop2: P2) extends ForeignKeyId0:
  type Left = P1
  type Right = P2
  val left: Left = prop1
  val right: Right = prop2

trait ForeignKeyBuilder extends RecordSchemaBuilderBase:
  extension [T, P1 <: SimplePropertyId[RecordType, T]](inline prop1: P1)
    transparent inline def foreignKey[R2, P2 <: SimplePropertyId[R2, T]](inline prop2: P2) =
      new ForeignKeyId[RecordType, R2, T, P1, P2](prop1, prop2){}
