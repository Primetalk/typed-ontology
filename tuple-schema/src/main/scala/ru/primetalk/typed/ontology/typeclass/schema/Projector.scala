package ru.primetalk.typed.ontology.typeclass.schema

/** Projector from Source schema to Dest schema exists only if such conversion is possible. In this
  * case it allows to perform the conversion of values.
  *
  * NB! In general, construction of projection requires `O(n^^2)` operations.
  */
type Projector[Source <: Tuple, SourceV <: Tuple, Dest <: Tuple, DestV <: Tuple] =
  Transformer[Source, RecordTupleValue[Source, SourceV], Dest, RecordTupleValue[Dest, DestV]]
