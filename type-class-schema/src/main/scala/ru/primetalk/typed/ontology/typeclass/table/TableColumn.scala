package ru.primetalk.typed.ontology.typeclass.table

import ru.primetalk.typed.ontology.typeclass.schema.*

abstract class TableColumn[Name <: String, V]

object TableColumn:

  given [Name1 <: String : ValueOf, V, C <: TableColumn[Name1, V]: ValueOf]: Column[C] =
    new:
      type Name = Name1
      def name(column: C): Name =
        valueOf[Name1]

  given [Name1 <: String, V, C <: TableColumn[Name1, V]: ValueOf]: SchemaValueType[C] =
    new:
      type Value = V

end TableColumn
