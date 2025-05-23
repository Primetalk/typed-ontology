package ru.primetalk.typed.ontology.typeclass.schema

import scala.language.experimental.namedTuples
import ru.primetalk.typed.ontology.typeclass.schema.RecordTupleValue.Prepend

@FunctionalInterface
trait TransUpdater[Change, ChangeV, Dest, DestV] extends (ChangeV => DestV => DestV):
  def apply(changeV: ChangeV): DestV => DestV

trait TransUpdaterLowPriority2:

  import RecordTupleValue.prepend

  given nonEmptyDestHeadTransUpdater[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using 
    getter: Getter[H, HV, RecordTupleValue[Source, SV]],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    transUpdater: TransUpdater[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): TransUpdater[Source, RecordTupleValue[Source, SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] =
    v => {
      case Prepend(_, tailDest) =>
        getter(v).value *: transUpdater(v)(tailDest).value
    }


end TransUpdaterLowPriority2

trait TransUpdaterLowPriority:
  given missingChangeColumnTransUpdater[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
    svts: RecordValueType[Source, SV],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    transUpdater: TransUpdater[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): TransUpdater[H *: Source, RecordTupleValue[H *: Source, HV *: SV], Dest, RecordTupleValue[Dest, DV]] = {
    case Prepend(_, tail) =>
      transUpdater(tail)
  }

  given missingDestColumnTransUpdater[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
    svts: RecordValueType[Source, SV],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    transUpdater: TransUpdater[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): TransUpdater[Source, RecordTupleValue[Source, SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] = changeV => {
    case Prepend(head, tail) =>
      head *: transUpdater(changeV)(tail).value
  }

  given nonEmptyHeadMatchTransUpdater[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
    svts: RecordValueType[Source, SV],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    transUpdater: TransUpdater[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): TransUpdater[H *: Source, RecordTupleValue[H *: Source, HV *: SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] = {
    case Prepend(headChange, tailChange) => {
      case Prepend(_, tailDest) =>
        headChange *: transUpdater(tailChange)(tailDest).value
    }
  }
end TransUpdaterLowPriority

object TransUpdater extends TransUpdaterLowPriority with TransUpdaterLowPriority2:
  given emptyChangeTransUpdater[Dest <: Tuple, DV <: Tuple](using
  svtd: RecordValueType[Dest, DV]
  ): TransUpdater[EmptyTuple, RecordTupleValue[EmptyTuple, EmptyTuple], Dest, RecordTupleValue[Dest, DV]] = _ => identity
  