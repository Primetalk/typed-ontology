package ru.primetalk.typed.ontology.typeclass.schema

/** Transformer from Source schema to Dest schema exists only if such conversion is possible. In
  * this case it allows to perform the conversion of values.
  */
@FunctionalInterface
trait Transformer[Source, -SourceV, Dest, +DestV] extends Function1[SourceV, DestV]:
  def apply(source: SourceV): DestV

trait ProjectionTransformerLowPriority2:

  import RecordTupleValue.prepend

  given nonEmptyDestHeadTransformer[
      H,
      HV,
      Source <: Tuple,
      SV <: Tuple,
      Dest <: Tuple,
      DV <: Tuple
  ](using
      getter: Getter[H, HV, RecordTupleValue[Source, SV]],
      svtd: RecordValueType[Dest, DV],
      hvt: SchemaValueType[H, HV],
      proj: Transformer[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): Transformer[Source, RecordTupleValue[Source, SV], H *: Dest, RecordTupleValue[
    H *: Dest,
    HV *: DV
  ]] =
    v => proj(v).prepend(getter(v))

end ProjectionTransformerLowPriority2

trait ProjectionTransformerLowPriority extends ProjectionTransformerLowPriority2:

  import RecordTupleValue.Prepend
  import RecordTupleValue.prepend

  /** An important case when order of elements is the same in Source and Dest */
  given nonEmptyHeadMatchTransformer[
      H,
      HV,
      Source <: Tuple,
      SV <: Tuple,
      Dest <: Tuple,
      DV <: Tuple
  ](using
      svts: RecordValueType[Source, SV],
      svtd: RecordValueType[Dest, DV],
      hvt: SchemaValueType[H, HV],
      proj: Transformer[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): Transformer[H *: Source, RecordTupleValue[H *: Source, HV *: SV], H *: Dest, RecordTupleValue[
    H *: Dest,
    HV *: DV
  ]] = { case Prepend(head, tail) =>
    proj(tail).prepend(head)
  }

  /** Another often seen case when source head is dropped. */
  given missingColumnTransformer[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](
      using
      svts: RecordValueType[Source, SV],
      svtd: RecordValueType[Dest, DV],
      hvt: SchemaValueType[H, HV],
      proj: Transformer[Source, RecordTupleValue[Source, SV], Dest, RecordTupleValue[Dest, DV]]
  ): Transformer[H *: Source, RecordTupleValue[H *: Source, HV *: SV], Dest, RecordTupleValue[
    Dest,
    DV
  ]] = { case Prepend(_, tail) =>
    proj(tail)
  }
end ProjectionTransformerLowPriority

object Transformer extends ProjectionTransformerLowPriority:

  /** Most important case when source = dest. */
  given identityTransformer[S <: Tuple, V <: Tuple](using
      svt: RecordValueType[S, V]
  ): Transformer[S, RecordTupleValue[S, V], S, RecordTupleValue[S, V]] =
    identity

  given identityReplacerTransformer[S <: Tuple, V <: Tuple, From, To, FromV](using
      svt: RecordValueType[S, V],
      fsvt: SchemaValueType[From, FromV],
      tsvt: SchemaValueType[To, FromV]
  ): ReplacerTransformer[S, V, From, To] =
    identity

end Transformer
