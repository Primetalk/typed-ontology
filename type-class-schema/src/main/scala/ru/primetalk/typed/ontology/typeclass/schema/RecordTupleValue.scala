package ru.primetalk.typed.ontology.typeclass.schema

import scala.annotation.targetName

/** Type-level annotation of a Tuple with it's RecordSchema.
 * Elements are connected via Column[T], SchemaValueType[C].
 */
opaque type RecordTupleValue[R <: Tuple, +V <: Tuple] >: V = V

/** Instance of this type-class only exists when there are corresponding Column[T], SchemaValueType[C]. */
final class ValidRecordTupleValue[R <: Tuple, V <: Tuple]

object RecordTupleValue:
  private val EmptyValidRecordTupleValue = new ValidRecordTupleValue[EmptyTuple, EmptyTuple]
  
  given emptyValidRecordTupleValue: ValidRecordTupleValue[EmptyTuple, EmptyTuple] =
    EmptyValidRecordTupleValue

  given nonEmptyValidRecordTupleValue[C: Column: ValueOf, V, S <: Tuple, T <: Tuple](using svt: SchemaValueType[C, V])(using ValidRecordTupleValue[S, T]): ValidRecordTupleValue[C *: S, V *: T] =
    new ValidRecordTupleValue[C *: S, V *: T]

  given [S <: Tuple, V <: Tuple](using vrtv: ValidRecordTupleValue[S, V]): SchemaValueType[S, RecordTupleValue[S, V]] =
    new SchemaValueType[S, RecordTupleValue[S, V]]
  
  extension [R <: Tuple, V <: Tuple](v: RecordTupleValue[R, V])
    inline def toTuple: V = v.asInstanceOf[V]
    inline def prepend[H, HV](vh: HV)(using SchemaValueType[H, HV]): RecordTupleValue[H *: R, HV *: V] =
      vh *: v.toTuple
      
    inline def get[Column, ColumnValue](using svt: SchemaValueType[Column, ColumnValue])(using getter: Getter[Column, ColumnValue, R, RecordTupleValue[R, V]]): ColumnValue =
      getter(v)

  object Prepend:
    def unapply[H, HV, R <: Tuple, V <: Tuple](r: RecordTupleValue[H *: R, HV *: V]): Option[(HV, RecordTupleValue[R, V])] =
      Some((r.toTuple.head, r.toTuple.tail))

  inline given dropGetter[OtherColumn, OtherColumnValue, Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple](using getter: Getter[Column, ColumnValue, Schema, SchemaValue]): Getter[Column, ColumnValue, OtherColumn *: Schema, OtherColumnValue *: SchemaValue] = {
    case Prepend(_, tail) =>
      getter(tail)
  }

  inline given headGetter[Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple]: Getter[Column, ColumnValue, Column *: Schema, ColumnValue *: SchemaValue] = {
    case Prepend(head, _) =>
      head
  }
