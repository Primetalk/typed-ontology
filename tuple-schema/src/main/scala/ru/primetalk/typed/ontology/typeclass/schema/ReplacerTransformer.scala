package ru.primetalk.typed.ontology.typeclass.schema

type ReplacerTransformer[Source <: Tuple, SourceV <: Tuple, From, To] =
  Transformer[Source, SourceV, Replace[Source, From, To], SourceV]
