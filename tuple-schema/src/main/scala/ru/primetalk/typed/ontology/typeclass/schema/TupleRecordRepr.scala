package ru.primetalk.typed.ontology.typeclass.schema

import scala.compiletime.*
import scala.language.experimental.namedTuples

class TupleRecordRepr[S <: Tuple, V <: Tuple] extends RecordRepr[S, V, V]:
  private inline def get0[S0 <: Tuple, V0 <: Tuple, HS, HV](
                                                             using svt: SchemaValueType[HS, HV],
                                                             ev: Tuple.Contains[S, HS] =:= true,
                                                             svt2: SchemaValueType[S0, RecordTupleValue[S0, V0]]
                                                   ): V0 => HV =
    inline erasedValue[S0] match
      case _: EmptyTuple => v => ???
      case _: (HS *: tail) => v => v.head.asInstanceOf[HV]
      case _: (_ *: tail) => v => get0(using svt, ev, summonInline[SchemaValueType[tail, RecordTupleValue[tail, Tuple.Tail[V0]]]])(v.tail)

  private inline def getOpt0[S0 <: Tuple, V0 <: Tuple, HS, HV](
                                                             using svt: SchemaValueType[HS, HV],
                                                             svt2: SchemaValueType[S0, V0]
                                                           ): V0 => Option[HV] =
    inline erasedValue[S0] match
      case _: EmptyTuple => v => None
      case _: (HS *: tail) => v => Some(v.head).asInstanceOf[Option[HV]]
      case _: (_ *: tail) => v => getOpt0(using svt, summonInline[SchemaValueType[tail, Tuple.Tail[V0]]])(v.tail)

  inline def get[HS, HV](
                          using svt: SchemaValueType[HS, HV],
                          ev: Tuple.Contains[S, HS] =:= true,
                          svt2: SchemaValueType[S, RecordTupleValue[S, V]]
                        ): V => HV = get0


  inline def getOpt[HS, HV](
                             using svt: SchemaValueType[HS, HV],
                             svt2: SchemaValueType[S, V]
                           ): V => Option[HV] = getOpt0

  inline def project[S2 <: Tuple, V2 <: Tuple](
                                                using svt: SchemaValueType[S2, V2],
                                                transformer: Transformer[S, V, S2, V2],
                                                svt2: SchemaValueType[S, V]
                                              ): V => V2 = transformer.apply

inline given tupleRecordRepr[S <: Tuple, V <: Tuple]: RecordRepr[S, V, V] =
  new TupleRecordRepr[S, V]

inline given getter[S <: Tuple, V <: Tuple, Column, ColumnValue](
                                                                         using recordRepr: RecordRepr[S, V, V],
                                                                         ev: Tuple.Contains[S, Column] =:= true,
                                                                         svt2: SchemaValueType[S, RecordTupleValue[S, V]],
                                                                         svt: SchemaValueType[Column, ColumnValue]
                                                                       ): Getter[Column, ColumnValue, V] =
  new Getter[Column, ColumnValue, V]:
    def apply(rtc: V): ValueWithSchema[Column, ColumnValue] =
      recordRepr.get[Column, ColumnValue](rtc): ValueWithSchema[Column, ColumnValue]
