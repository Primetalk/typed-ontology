package ru.primetalk.typed.ontology.typeclass.fschema

import RecordTupleValueF.Prepend
import ru.primetalk.typed.ontology.typeclass.schema.{RecordValueType, SchemaValueType}

import scala.language.experimental.namedTuples

trait TransUpdaterF[F[_], Change, ChangeV <: Tuple, Dest, DestV] extends (Tuple.Map[ChangeV, F] => DestV => F[DestV]):
  def apply(changeV: Tuple.Map[ChangeV, F]): DestV => F[DestV]

trait TransUpdaterFLowPriority2:

  import RecordTupleValueF.prepend

  given nonEmptyDestHeadTransUpdaterF[F[_], H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                      getterF: GetterF[F, H, HV, RecordTupleValueF[F, Source, SV]],
                                                                                                      svtd: RecordValueType[Dest, DV],
                                                                                                      hvt: SchemaValueType[H, HV],
                                                                                                      transUpdaterF: TransUpdaterF[F, Source, RecordTupleValueF[F, Source, SV], Dest, RecordTupleValueF[F, Dest, DV]]
                                                                                                     ): TransUpdaterF[F, Source, RecordTupleValueF[F, Source, SV], H *: Dest, RecordTupleValueF[F, H *: Dest, HV *: DV]] =
    v => {
      case Prepend(headDest, tailDest: RecordTupleValueF[F, Dest, DV]) => (
        getterF(v) match
          case Some(hv: HV) => hv
          case None => headDest
        ) *: transUpdaterF(v)(tailDest)
    }


end TransUpdaterFLowPriority2

trait TransUpdaterFLowPriority:
  given missingChangeColumnTransUpdaterF[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                         svts: RecordValueType[Source, SV],
                                                                                                         svtd: RecordValueType[Dest, DV],
                                                                                                         hvt: SchemaValueType[H, HV],
                                                                                                         transUpdaterF: TransUpdaterF[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                        ): TransUpdaterF[H *: Source, RecordTupleValue[H *: Source, HV *: SV], Dest, RecordTupleValue[Dest, DV]] = {
    case Prepend(_, tail) =>
      transUpdaterF(tail)
  }

  given missingDestColumnTransUpdaterF[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                       svts: RecordValueType[Source, SV],
                                                                                                       svtd: RecordValueType[Dest, DV],
                                                                                                       hvt: SchemaValueType[H, HV],
                                                                                                       transUpdaterF: TransUpdaterF[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                      ): TransUpdaterF[Source, RecordTupleValue[Source, SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] = changeV => {
    case Prepend(head, tail) =>
      head *: transUpdaterF(changeV)(tail).value
  }

  given nonEmptyHeadMatchTransUpdaterF[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
                                                                                                       svts: RecordValueType[Source, SV],
                                                                                                       svtd: RecordValueType[Dest, DV],
                                                                                                       hvt: SchemaValueType[H, HV],
                                                                                                       transUpdaterF: TransUpdaterF[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
                                                                                                      ): TransUpdaterF[H *: Source, RecordTupleValue[H *: Source, HV *: SV], H *: Dest, RecordTupleValue[H *: Dest, HV *: DV]] = {
    case Prepend(headChange, tailChange) => {
      case Prepend(_, tailDest) =>
        headChange *: transUpdaterF(tailChange)(tailDest).value
    }
  }
end TransUpdaterFLowPriority

object TransUpdaterF extends TransUpdaterFLowPriority with TransUpdaterFLowPriority2:
  given emptyChangeTransUpdaterF[Dest <: Tuple, DV <: Tuple](using
                                                            svtd: RecordValueType[Dest, DV]
                                                           ): TransUpdaterF[EmptyTuple, RecordTupleValue[EmptyTuple, EmptyTuple], Dest, RecordTupleValue[Dest, DV]] = _ => identity
