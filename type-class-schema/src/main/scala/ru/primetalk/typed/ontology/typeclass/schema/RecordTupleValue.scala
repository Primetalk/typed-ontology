package ru.primetalk.typed.ontology.typeclass.schema

/** Type-level annotation of a Tuple with it's RecordSchema.
 * Elements are connected via Column[T], SchemaValueType[C].
 */
opaque type RecordTupleValue[R <: Tuple, +V <: Tuple] >: V = V

/** Instance of this type-class only exists when there are corresponding Column[T], SchemaValueType[C]. */
trait ValidRecordTupleValue[R <: Tuple, V <: Tuple]
