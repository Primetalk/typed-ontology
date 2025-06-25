package ru.primetalk.typed.ontology.typeclass.fschema

import ru.primetalk.typed.ontology.typeclass.schema.RecordTupleValue.Prepend
import ru.primetalk.typed.ontology.typeclass.schema.*

import scala.language.experimental.namedTuples

trait TransUpdaterOpt[Change, ChangeV <: Tuple, Dest, DestV] extends (ChangeV => DestV => DestV):
  def apply(changeV: Tuple.Map[ChangeV, Option]): DestV => DestV

trait TransUpdaterOptLowPriority2:

  import RecordTupleValue.prepend

  given nonEmptyDestHeadTransUpdaterOpt[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                      getter: Getter[H, HV, RecordTupleValue[Source, SV]],
                                                                                                      svtd: RecordValueType[Dest, DV],
                                                                                                      hvt: SchemaValueType[H, HV],
                                                                                                      transUpdaterOpt: TransUpdaterOpt[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                     ): TransUpdaterOpt[Source, RecordTupleValue[Source, SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] =
    v => {
      case Prepend(headDest, tailDest) => (
        getter(v).value match
          case Some(hv: HV) => hv
          case None => headDest
        ) *: transUpdaterOpt(v)(tailDest).value
    }


end TransUpdaterOptLowPriority2

trait TransUpdaterOptLowPriority:
  given missingChangeColumnTransUpdaterOpt[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                         svts: RecordValueType[Source, SV],
                                                                                                         svtd: RecordValueType[Dest, DV],
                                                                                                         hvt: SchemaValueType[H, HV],
                                                                                                         transUpdaterOpt: TransUpdaterOpt[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                        ): TransUpdaterOpt[H *: Source, RecordTupleValue[H *: Source, HV *: SV], Dest, RecordTupleValue[Dest, DV]] = {
    case Prepend(_, tail) =>
      transUpdaterOpt(tail)
  }

  given missingDestColumnTransUpdaterOpt[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                       svts: RecordValueType[Source, SV],
                                                                                                       svtd: RecordValueType[Dest, DV],
                                                                                                       hvt: SchemaValueType[H, HV],
                                                                                                       transUpdaterOpt: TransUpdaterOpt[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                      ): TransUpdaterOpt[Source, RecordTupleValue[Source, SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] = changeV => {
    case Prepend(head, tail) =>
      head *: transUpdaterOpt(changeV)(tail).value
  }

  given nonEmptyHeadMatchTransUpdaterOpt[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                       svts: RecordValueType[Source, SV],
                                                                                                       svtd: RecordValueType[Dest, DV],
                                                                                                       hvt: SchemaValueType[H, HV],
                                                                                                       transUpdaterOpt: TransUpdaterOpt[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                      ): TransUpdaterOpt[H *: Source, RecordTupleValue[H *: Source, HV *: SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] = {
    case Prepend(headChange, tailChange) => {
      case Prepend(_, tailDest) =>
        headChange *: transUpdaterOpt(tailChange)(tailDest).value
    }
  }
end TransUpdaterOptLowPriority

object TransUpdaterOpt extends TransUpdaterOptLowPriority with TransUpdaterOptLowPriority2:
  given emptyChangeTransUpdaterOpt[Dest <: Tuple, DV <: Tuple](using
                                                            svtd: RecordValueType[Dest, DV]
                                                           ): TransUpdaterOpt[EmptyTuple, RecordTupleValue[EmptyTuple, EmptyTuple], Dest, RecordTupleValue[Dest, DV]] = _ => identity
