package ru.primetalk.typed.ontology.typeclass.schema

import scala.annotation.{implicitNotFound, targetName}
import scala.NamedTuple.*
import scala.NamedTupleDecomposition.*
import scala.compiletime.summonInline

/** Type-level annotation of a Tuple with it's RecordSchema. Elements are connected via Column[T],
  * SchemaValueType[C].
  */
type RecordTupleValue[R <: Tuple, +V <: Tuple] = ValueWithSchema[R, V]

/** Instance of this type-class only exists when there are corresponding Column[T],
  * SchemaValueType[C].
  */
@implicitNotFound(msg = "Cannot prove that ${V} is a valid primitive value of schema ${R}.")
final class ValidRecordTupleValue[R <: Tuple, V <: Tuple]

object RecordTupleValue:
  final class RecordTupleValueApi[R <: Tuple]:
    def apply[V <: Tuple](v: V)(using ValidRecordTupleValue[R, V]): RecordTupleValue[R, V] =
      v

  def apply[R <: Tuple]: RecordTupleValueApi[R] = new RecordTupleValueApi

  final class FromNamedTupleApi[R <: Tuple]:
    inline def apply[V <: Tuple, Names <: Tuple](t: NamedTuple[Names, V])(using
        columnsNames: ColumnsNames[R, Names],
        svt: ValidRecordTupleValue[R, V]
    ): RecordTupleValue[R, V] =
      RecordTupleValue[R](t.toTuple)

  inline def fromNamedTuple[R <: Tuple]: FromNamedTupleApi[R] = new FromNamedTupleApi

  private val EmptyValidRecordTupleValue = new ValidRecordTupleValue[EmptyTuple, EmptyTuple]

  given emptyValidRecordTupleValue: ValidRecordTupleValue[EmptyTuple, EmptyTuple] =
    EmptyValidRecordTupleValue

  given nonEmptyValidRecordTupleValue[C: Column: ValueOf, V, S <: Tuple, T <: Tuple](using
      svt: SchemaValueType[C, V]
  )(using ValidRecordTupleValue[S, T]): ValidRecordTupleValue[C *: S, V *: T] =
    new ValidRecordTupleValue[C *: S, V *: T]

  given svtFromValidRecordTupleValue[S <: Tuple, V <: Tuple](using
      vrtv: ValidRecordTupleValue[S, V]
  ): SchemaValueType[S, RecordTupleValue[S, V]] =
    new SchemaValueType[S, RecordTupleValue[S, V]]

  extension [R <: Tuple](r: R)
    inline def fromNamedTuple[V <: Tuple, Names <: Tuple](t: NamedTuple[Names, V])(using
        columnsNames: ColumnsNames[R, Names],
        svt: ValidRecordTupleValue[R, V]
    ): RecordTupleValue[R, V] =
      RecordTupleValue[R](t.toTuple)

  extension [R <: Tuple, V <: Tuple](v: RecordTupleValue[R, V])

    inline def toTuple: V = v.asInstanceOf[V]

    inline def toNamedTuple[Names <: Tuple](using
        columnsNames: ColumnsNames[R, Names]
    ): NamedTuple[Names, V] =
      v.asInstanceOf[NamedTuple[Names, V]]

    inline def prepend[H, HV](vh: ValueWithSchema[H, HV]): RecordTupleValue[H *: R, HV *: V] =
      vh.value *: v.toTuple

    inline def get[Column](column: Column)[ColumnValue](using
        svt: SchemaValueType[Column, ColumnValue],
        getter: Getter[Column, ColumnValue, RecordTupleValue[R, V]]): ColumnValue =
      getter(v).value

    transparent inline def apply[Column]()[ColumnValue](using
        svt: SchemaValueType[Column, ColumnValue],
        getter: Getter[Column, ColumnValue, RecordTupleValue[R, V]]): ColumnValue =
      getter(v).value

    inline def project[Dest <: Tuple, DestV <: Tuple](dest: Dest)(using
        proj: Projector[R, V, Dest, DestV]
    ): RecordTupleValue[Dest, DestV] =
      proj.apply(v)

    inline def as[P <: Product](using m: scala.deriving.Mirror.ProductOf[P]): P =
      m.fromProduct(v.toTuple)

    inline def toSelectable[Names <: Tuple](using columnsNames: ColumnsNames[R, Names]): SelectableRTV[R, V, Names] = 
      SelectableRTV[R, V, Names](v)

  extension [V <: Tuple](v: V)
    inline def asRecord[S <: Tuple](using
        svt: SchemaValueType[S, RecordTupleValue[S, V]]
    ): svt.Value =
      v: svt.Value

  extension [P <: Product](p: P)
    inline def asRecord[S <: Tuple](using m: scala.deriving.Mirror.ProductOf[P])(using
        svt: SchemaValueType[S, RecordTupleValue[S, m.MirroredElemTypes]]
    ): svt.Value =
      Tuple.fromProductTyped[P](p).asRecord[S]

  object Prepend:
    def unapply[H, HV, R <: Tuple, V <: Tuple](
        r: RecordTupleValue[H *: R, HV *: V]
    ): (HV, RecordTupleValue[R, V]) =
      (r.toTuple.head, r.toTuple.tail)

  inline given dropGetter[
      OtherColumn,
      OtherColumnValue,
      Column,
      ColumnValue,
      Schema <: Tuple,
      SchemaValue <: Tuple
  ](using
      getter: Getter[Column, ColumnValue, RecordTupleValue[Schema, SchemaValue]]
  ): Getter[Column, ColumnValue, RecordTupleValue[
    OtherColumn *: Schema,
    OtherColumnValue *: SchemaValue
  ]] = { case Prepend(_, tail) =>
    getter(tail)
  }

  inline given headGetter[Column, ColumnValue, Schema <: Tuple, SchemaValue <: Tuple]: Getter[
    Column,
    ColumnValue,
    RecordTupleValue[Column *: Schema, ColumnValue *: SchemaValue]
  ] = { case Prepend(head, _) =>
    head
  }

  /**
    * Find a corresponding type in the target tuple using index tuple as a lookup.
    */
  type CorrespondingType[Index <: Tuple, IndexValue, Target <: Tuple] =
    Index match
      case IndexValue *: tail => 
        Target match 
          case head *: _ => head
          case _ => Nothing
      case _ *: tail =>
        Target match
          case _ *: ttail => CorrespondingType[tail, IndexValue, ttail]
          case _ => Nothing
      case _ =>
        Nothing 

  class SelectableRTV[R <: Tuple, V <: Tuple, Names <: Tuple](
    val record: RecordTupleValue[R, V])(using columnsNames: ColumnsNames[R, Names]) 
    extends scala.Selectable:

    type Fields = NamedTuple[Names, V]
    
    inline def selectDynamic(name: String): Any = 
      type Column = CorrespondingType[Names, name.type, R]
      type ColumnValue = CorrespondingType[Names, name.type, V]
      val getter = summonInline[Getter[Column, ColumnValue, RecordTupleValue[R, V]]]
      getter(record).value

  inline given [R <: Tuple, V <: Tuple, Names <: Tuple](using
        columnsNames: ColumnsNames[R, Names]
    ) : Conversion[RecordTupleValue[R, V], Selectable] with
    def apply(v: RecordTupleValue[R, V]): Selectable = 
      new SelectableRTV[R, V, Names](v)
