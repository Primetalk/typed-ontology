package ru.primetalk.typed.ontology.typeclass.schema

import scala.language.experimental.namedTuples

type ReplacerTransformer[Source <: Tuple, SourceV <: Tuple, From, To] = 
  Transformer[Source, SourceV, Replace[Source, From, To], SourceV]
