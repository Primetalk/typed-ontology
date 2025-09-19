package ru.primetalk.typed.ontology.typeclass.schema

import scala.compiletime.*
import scala.language.experimental.namedTuples

class RecordTupleValueRepr[S <: Tuple, V <: Tuple] extends RecordRepr[S, V, RecordTupleValue[S, V]]:
  private inline def get0[S0 <: Tuple, V0 <: Tuple, HS, HV](
                                                             using svt: SchemaValueType[HS, HV],
                                                             ev: Tuple.Contains[S, HS] =:= true,
                                                             svt2: SchemaValueType[S0, RecordTupleValue[S0, V0]]
                                                   ): V0 => HV =
    inline erasedValue[S0] match
      case _: EmptyTuple => v => ???
      case _: (HS *: tail) => v => v.head.asInstanceOf[HV]
      case _: (_ *: tail) =>
        val f = get0(using svt, ev, summonInline[SchemaValueType[tail, RecordTupleValue[tail, Tuple.Tail[V0]]]])
        v => f(v.tail)

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
                        ): RecordTupleValue[S, V] => HV = v => get0[S, V, HS, HV](v.value)


  inline def getOpt[HS, HV](
                             using svt: SchemaValueType[HS, HV],
                             svt2: SchemaValueType[S, V]
                           ): RecordTupleValue[S, V] => Option[HV] = v => getOpt0[S, V, HS, HV](v.value)

  inline def project[S2 <: Tuple, V2 <: Tuple](
                                                using svt: SchemaValueType[S2, V2],
                                                transformer: Transformer[S, V, S2, V2],
                                                svt2: SchemaValueType[S, V]
                                              ): RecordTupleValue[S, V] => V2 = v => transformer.apply(v.value)

inline given recordTupleValueRepr[S <: Tuple, V <: Tuple]: RecordRepr[S, V, RecordTupleValue[S, V]] =
  new RecordTupleValueRepr[S, V]

inline given getter2[S <: Tuple, V <: Tuple, Column, ColumnValue](
                                                                         using recordRepr: RecordRepr[S, V, RecordTupleValue[S, V]],
                                                                         ev: Tuple.Contains[S, Column] =:= true,
                                                                         svt2: SchemaValueType[S, RecordTupleValue[S, V]],
                                                                         svt: SchemaValueType[Column, ColumnValue]
                                                                       ): Getter[Column, ColumnValue, RecordTupleValue[S, V]] =
  {
    val f = recordRepr.get[Column, ColumnValue](using svt, ev, svt2)
    new Getter[Column, ColumnValue, RecordTupleValue[S, V]]:
      def apply(rtc: RecordTupleValue[S, V]): ValueWithSchema[Column, ColumnValue] =
        f(rtc): ValueWithSchema[Column, ColumnValue]
    }
