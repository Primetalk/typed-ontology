package ru.primetalk.typed.ontology.typeclass.schema

/** @tparam C is a column that could be part of RecordSchema. */
trait Column[C]:
  type Name <: String
  // def name: Name

object Column:
  inline def infer[Col: ValueOf]: Col =
    valueOf[Col]

  // inline def nameOf[C](using col: Column[C]): col.Name =
  //   col.name

//  type NameOf[C] <: String = C match
//    case {type Name <: String} => c#Name
//    case _ => Nothing
  type Columns[Schema <: Tuple] = Tuple.Map[Schema, Column]
//  type ColumnNames[Schema <: Tuple] = Tuple.Map[Tuple.Map[Schema, Column], NameOf]

  inline def columnNames[Schema <: Tuple](using columns: Columns[Schema]): columns.type =
    columns
