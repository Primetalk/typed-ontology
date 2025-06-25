package ru.primetalk.typed.ontology.typeclass.fschema

import cats.Id
import ru.primetalk.typed.ontology.typeclass.schema.{Column, ColumnsNames, SchemaValueType, ValueWithSchema}

import scala.annotation.{implicitNotFound, targetName}
import scala.language.experimental.namedTuples
import scala.NamedTuple.*
import scala.NamedTupleDecomposition.*

/** Type-level annotation of a Tuple with it's RecordSchema.
 * Elements are connected via Column[T], SchemaValueType[C].
 */
opaque type RecordTupleValueF[F[_], R <: Tuple, +V <: Tuple] >: Tuple.Map[V, F] <: Tuple = Tuple.Map[V, F]

type RecordTupleValue[R <: Tuple, +V <: Tuple] = RecordTupleValueF[Id, R, V]

/** Instance of this type-class only exists when there are corresponding Column[T], SchemaValueType[C]. */
@implicitNotFound(msg = "Cannot prove that $V is a valid primitive value of schema $R.")
final class ValidRecordTupleValueF[F[_], R <: Tuple, V <: Tuple]

object RecordTupleValueF:
  private class IdentityConversion[A] extends Conversion[A, A]:
    def apply(x: A): A = x

  inline given valueToRecordTupleValueF[F[_], Schema <: Tuple, Value <: Tuple](using ev: SchemaValueType[Schema, Value]): Conversion[Tuple.Map[Value, F], RecordTupleValueF[F, Schema, Value]] =
    new IdentityConversion[RecordTupleValueF[F, Schema, Value]]

  extension [F[_], Schema <: Tuple, Value <: Tuple](vws: RecordTupleValueF[F, Schema, Value])
    def value: Tuple.Map[Value, F] = vws
  
  given emptyValidRecordTupleValue[F[_]]: ValidRecordTupleValueF[F, EmptyTuple, EmptyTuple] =
    new ValidRecordTupleValueF

  given nonEmptyValidRecordTupleValueF[F[_], C: Column: ValueOf, V, S <: Tuple, T <: Tuple](using svt: SchemaValueType[C, V])(using ValidRecordTupleValueF[F, S, T]): ValidRecordTupleValueF[F, C *: S, F[V] *: T] =
    new ValidRecordTupleValueF

//  given svtFromValidRecordTupleValueF[F[_], S <: Tuple, V <: Tuple](using vrtv: ValidRecordTupleValueF[F, S, V]): SchemaValueType[S, RecordTupleValueF[F, S, V]] =
//    new SchemaValueType

  extension [F[_], R <: Tuple, V <: Tuple](v: RecordTupleValueF[F, R, V])
    inline def toTuple: Tuple.Map[V, F] = v.asInstanceOf[Tuple.Map[V, F]]

    inline def toNamedTuple[Names <: Tuple](using columnsNames: ColumnsNames[R, Names]): NamedTuple[Names, V] =
      v.asInstanceOf[NamedTuple[Names, V]]

    inline def prepend[H, HV](vh: F[HV])(using svt: SchemaValueType[H, HV]): RecordTupleValueF[F, H *: R, HV *: V] =
      vh *: v.toTuple

    inline def get[Column, ColumnValue](column: Column)(
      using svt: SchemaValueType[Column, ColumnValue]
    )(using getter: GetterF[F, Column, ColumnValue, V]): F[ValueWithSchema[Column, ColumnValue]] =
      getter(v)

    inline def project[Dest <: Tuple, DestV <: Tuple](dest: Dest)(using proj: ProjectorF[F, R, V, Dest, DestV]): F[RecordTupleValueF[F, Dest, DestV]] =
      proj.apply(v)

  object Prepend:
    def unapply[F[_], H, HV, R <: Tuple, V <: Tuple](r: RecordTupleValueF[F, H *: R, HV *: V]): (F[HV], RecordTupleValueF[F, R, V]) =
      (r.toTuple.head, r.toTuple.tail)

  inline given dropGetter[OtherColumn, OtherColumnValue, Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple](using getter: Getter[Column, ColumnValue, RecordTupleValueF[F[_], Schema, SchemaValue]]): Getter[Column, ColumnValue, RecordTupleValueF[F[_], OtherColumn *: Schema, OtherColumnValue *: SchemaValue]] = {
    case Prepend(_, tail) =>
      getter(tail)
  }
//
//  inline given headGetter[Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple]: Getter[Column, ColumnValue, RecordTupleValueF[F[_], Column *: Schema, ColumnValue *: SchemaValue]] = {
//    case Prepend(head, _) =>
//      head
//  }
