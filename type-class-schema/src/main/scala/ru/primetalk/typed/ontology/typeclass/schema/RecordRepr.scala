package ru.primetalk.typed.ontology.typeclass.schema

import scala.language.experimental.namedTuples

trait RecordRepr[S <: Tuple, V <: Tuple, -Repr]:
  inline def get[HS, HV](
                          using svt: SchemaValueType[HS, HV],
                          ev: Tuple.Contains[S, HS] =:= true,
                          svt2: SchemaValueType[S, RecordTupleValue[S, V]]
                        ): Repr => HV

  inline def getOpt[HS, HV](
                          using svt: SchemaValueType[HS, HV],
                          svt2: SchemaValueType[S, V]
                        ): Repr => Option[HV]
  
  inline def project[S2 <: Tuple, V2 <: Tuple](
                                              using svt: SchemaValueType[S2, V2],
                                              transformer: Transformer[S, V, S2, V2],
                                              svt2: SchemaValueType[S, V]
                                              ): Repr => V2
