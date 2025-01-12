package ru.primetalk.typed.ontology.typeclass.table

import ru.primetalk.typed.ontology.typeclass.schema.*

abstract class TableColumn[Name <: String, V]

object TableColumn:

  class ColumnImpl[Name1 <: String : ValueOf, V, C <: TableColumn[Name1, V]: ValueOf] extends Column[C]:
    type Name = Name1

    def name(column: C): Name =
      valueOf[Name1]

  inline given [Name1 <: String : ValueOf, V, C <: TableColumn[Name1, V]: ValueOf]: Column[C] =
    new ColumnImpl[Name1, V, C]

  inline given [Name1 <: String, V, C <: TableColumn[Name1, V]: ValueOf]: SchemaValueType[C, V] =
    new SchemaValueType[C, V]

end TableColumn
