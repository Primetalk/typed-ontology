package ru.primetalk.typed.ontology.typeclass.schema

import scala.language.experimental.namedTuples

/** 
 * Evidence that V is the underlying tuple for schema R.
 */
type RecordValueType[R <: Tuple, V <: Tuple] = SchemaValueType[R, RecordTupleValue[R, V]]
