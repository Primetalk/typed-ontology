package ru.primetalk.typed.ontology.simple.meta

import SimpleTypes.{given, *}

object Product extends TableBuilder:
  object id    extends column[Int]
  val id1 = id
  type Id = id1.type 
  object name  extends column[String]
  type Name = name.type
  object price extends column[BigInt]
  type Price = price.type

  type PriceSchema = Price #: EmptySchema
  type TableSchema = Id #: Name #: Price #: EmptySchema
  
  implicit val tableSchema: TableSchema = fields(id, name, price)
  val idNameSchema                      = fields(id, name)
  val primaryKeySchema                  = fields(id)

  val fullSchema = infer[TableSchema]
  val priceP = summon[RecordPropertyValueType.Aux1[Price]]
  val svt = summon[RecordSchemaValueType.Aux1[TableSchema]]
  type Row = svt.Value
end Product
