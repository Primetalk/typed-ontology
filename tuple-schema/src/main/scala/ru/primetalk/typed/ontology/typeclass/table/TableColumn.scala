package ru.primetalk.typed.ontology.typeclass.table

import ru.primetalk.typed.ontology.typeclass.schema.*

class TableColumn[Name <: String & Singleton, V]

object TableColumn:

  class ColumnImpl[Name1 <: String & Singleton : ValueOf, V, C <: TableColumn[Name1, V]: ValueOf] extends Column[C]:
    type Name = Name1

    def name(column: C): Name =
      valueOf[Name1]

  end ColumnImpl
      
  inline given [Name1 <: String & Singleton : ValueOf, V, C <: TableColumn[Name1, V]: ValueOf]: Column[C] =
    new ColumnImpl[Name1, V, C]

  inline given svtForColumn[Name1 <: String & Singleton, V, C <: TableColumn[Name1, V]: ValueOf]: SchemaValueType[C, V] =
    new SchemaValueType[C, V]

  given columnName[Name <: String & Singleton, V, C <: TableColumn[Name, V]]: ColumnName[C, Name] =
    new ColumnName[C, Name] {}

end TableColumn
