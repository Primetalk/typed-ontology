package ru.primetalk.typed.ontology.typeclass.relalg

import ru.primetalk.typed.ontology.typeclass.schema.SchemaValueType

case class ForeignKey[Parent <: Tuple, Child <: Tuple, Value <: Tuple](
                                      parent: Parent,
                                      child: Child
                                    )(
  using svtParent: SchemaValueType[Parent, Value],
  svtChild: SchemaValueType[Child, Value]
)
