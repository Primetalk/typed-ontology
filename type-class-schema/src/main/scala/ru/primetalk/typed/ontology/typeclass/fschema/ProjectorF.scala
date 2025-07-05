package ru.primetalk.typed.ontology.typeclass.fschema

import scala.language.experimental.namedTuples

/** Projector from Source schema to Dest schema exists only if such conversion is possible. In this
  * case it allows to perform the conversion of values.
  *
  * NB! In general, construction of projection requires `O(n^^2)` operations.
  */
type ProjectorF[F[_], Source <: Tuple, SourceV <: Tuple, Dest <: Tuple, DestV <: Tuple] =
  TransformerF[F, Source, RecordTupleValueF[F, Source, SourceV], Dest, RecordTupleValueF[F, Dest, DestV]]
