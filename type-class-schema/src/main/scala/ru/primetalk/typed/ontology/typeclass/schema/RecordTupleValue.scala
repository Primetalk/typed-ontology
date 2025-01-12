package ru.primetalk.typed.ontology.typeclass.schema

import scala.annotation.{implicitNotFound, targetName}

/** Type-level annotation of a Tuple with it's RecordSchema.
 * Elements are connected via Column[T], SchemaValueType[C].
 */
opaque type RecordTupleValue[R <: Tuple, +V <: Tuple] >: V = V

/** Instance of this type-class only exists when there are corresponding Column[T], SchemaValueType[C]. */
@implicitNotFound(msg = "Cannot prove that $V is a valid primitive value of schema $R.")
final class ValidRecordTupleValue[R <: Tuple, V <: Tuple]

object RecordTupleValue:
  private val EmptyValidRecordTupleValue = new ValidRecordTupleValue[EmptyTuple, EmptyTuple]
  
  given emptyValidRecordTupleValue: ValidRecordTupleValue[EmptyTuple, EmptyTuple] =
    EmptyValidRecordTupleValue

  given nonEmptyValidRecordTupleValue[C: Column: ValueOf, V, S <: Tuple, T <: Tuple](using svt: SchemaValueType[C, V])(using ValidRecordTupleValue[S, T]): ValidRecordTupleValue[C *: S, V *: T] =
    new ValidRecordTupleValue[C *: S, V *: T]

  given svtFromValidRecordTupleValue[S <: Tuple, V <: Tuple](using vrtv: ValidRecordTupleValue[S, V]): SchemaValueType[S, RecordTupleValue[S, V]] =
    new SchemaValueType[S, RecordTupleValue[S, V]]
  
  extension [R <: Tuple, V <: Tuple](v: RecordTupleValue[R, V])
    inline def toTuple: V = v.asInstanceOf[V]
    inline def prepend[H, HV](vh: HV)(using SchemaValueType[H, HV]): RecordTupleValue[H *: R, HV *: V] =
      vh *: v.toTuple
      
    inline def get[Column, ColumnValue](column: Column)(using svt: SchemaValueType[Column, ColumnValue])(using getter: Getter[Column, ColumnValue, RecordTupleValue[R, V]]): ColumnValue =
      getter(v)

    inline def project[Dest <: Tuple, DestV <: Tuple](dest: Dest)(using proj: Projector[R, V, Dest, DestV]): RecordTupleValue[Dest, DestV] =
      proj.apply(v)
      
  object Prepend:
    def unapply[H, HV, R <: Tuple, V <: Tuple](r: RecordTupleValue[H *: R, HV *: V]): (HV, RecordTupleValue[R, V]) =
      (r.toTuple.head, r.toTuple.tail)

  inline given dropGetter[OtherColumn, OtherColumnValue, Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple](using getter: Getter[Column, ColumnValue, RecordTupleValue[Schema, SchemaValue]]): Getter[Column, ColumnValue, RecordTupleValue[OtherColumn *: Schema, OtherColumnValue *: SchemaValue]] = {
    case Prepend(_, tail) =>
      getter(tail)
  }

  inline given headGetter[Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple]: Getter[Column, ColumnValue, RecordTupleValue[Column *: Schema, ColumnValue *: SchemaValue]] = {
    case Prepend(head, _) =>
      head
  }
