package ru.primetalk.typed.ontology.typeclass.schema

import scala.language.experimental.namedTuples

@FunctionalInterface
sealed trait Replacer[S <: Tuple, FROM, TO, V <: Tuple]:
  def apply(source: RecordTupleValue[S, V]): RecordTupleValue[Replace[S, FROM, TO], V]

object Replacer:

  inline given matchingReplacer[S <: Tuple, FROM, TO, HV, V <: Tuple](using
                                                  svt: RecordValueType[S, V],
                                                  fHvt: SchemaValueType[FROM, HV],
                                                  tHvt: SchemaValueType[TO, HV],
                                                 ): Replacer[S, FROM, TO, V] with
    def apply(v: RecordTupleValue[S, V]): RecordTupleValue[Replace[S, FROM, TO], V] =
        v.asInstanceOf[RecordTupleValue[Replace[S, FROM, TO], V]]

end Replacer
