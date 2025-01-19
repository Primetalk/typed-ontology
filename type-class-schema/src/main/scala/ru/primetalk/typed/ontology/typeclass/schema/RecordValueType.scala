package ru.primetalk.typed.ontology.typeclass.schema

import scala.language.experimental.namedTuples

type RecordValueType[R <: Tuple, V <: Tuple] = SchemaValueType[R, RecordTupleValue[R, V]]

//object RecordValueType:
//  type Aux[R <: Tuple, V <: Tuple] = RecordValueType[R] {type Value = V}
