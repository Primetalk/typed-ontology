package ru.primetalk.typed.ontology.typeclass.schema

/**
  * This type class creates a relation between a column and it's name.
  */
trait ColumnName[C, N <: String]

/**
  * This type class creates a correspondence between a list of columns and a list of names.
  */
sealed abstract class ColumnsNames[C <: Tuple, N <: Tuple]

object ColumnsNames:

  given emptyColumnsNames: ColumnsNames[EmptyTuple, EmptyTuple] = new ColumnsNames[EmptyTuple, EmptyTuple] {}

  given nonEmptyColumnsNames[C, CS <: Tuple, N <: String, NS <: Tuple](using ColumnName[C, N], ColumnsNames[CS, NS]): ColumnsNames[C *: CS, N *: NS] =
    new ColumnsNames[C *: CS, N *: NS] {}

end ColumnsNames
