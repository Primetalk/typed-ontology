package ru.primetalk.typed.ontology.typeclass.schema

/** Projector from Source schema to Dest schema exists only if such conversion is possible. In this
  * case it allows to perform the conversion of values.
  *
  * NB! In general, construction of projection requires `O(n^^2)` operations.
  */
@FunctionalInterface
trait Projector[Source <: Tuple, SourceV <: Tuple, Dest <: Tuple, DestV <: Tuple]:
  def apply(source: RecordTupleValue[Source, SourceV]): RecordTupleValue[Dest, DestV]

trait ProjectorLowPriority2:

  import RecordTupleValue.prepend

  given nonEmptyDestHeadProjector[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
    getter: Getter[H, HV, RecordTupleValue[Source, SV]],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    proj: Projector[Source, SV, Dest, DV]
  ): Projector[Source, SV, H *: Dest, HV *: DV] =
    v => proj(v).prepend(getter(v))


end ProjectorLowPriority2

trait ProjectorLowPriority extends ProjectorLowPriority2:

  import RecordTupleValue.Prepend
  import RecordTupleValue.prepend
  /** An important case when order of elements is the same in Source and Dest */
  given nonEmptyHeadMatchProjector[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
    svts: RecordValueType[Source, SV],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    proj: Projector[Source, SV, Dest, DV]
  ): Projector[H *: Source, HV *: SV, H *: Dest, HV *: DV] = { case Prepend(head, tail) =>
    proj(tail).prepend(head)
  }

  /** Another often seen case when source head is dropped. */
  given missingColumnProjector[H, HV, Source <: Tuple, SV <: Tuple, Dest <: Tuple, DV <: Tuple](using
    svts: RecordValueType[Source, SV],
    svtd: RecordValueType[Dest, DV],
    hvt: SchemaValueType[H, HV],
    proj: Projector[Source, SV, Dest, DV]
  ): Projector[H *: Source, HV *: SV, Dest, DV] = { case Prepend(_, tail) =>
    proj(tail)
  }
end ProjectorLowPriority

object Projector extends ProjectorLowPriority:

  /** Most important case when source = dest.  */
  given identityProjector[S <: Tuple, V <: Tuple](using
      svt: RecordValueType[S, V]
  ): Projector[S, V, S, V] =
    identity

end Projector
