/**
  * Package `simplemeta2` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simplemeta3

import scala.language.higherKinds
import ru.primetalk.typed.ontology.Record
import scala.quoted.*
import scala.reflect.ClassTag
import scala.compiletime.ops.int.S
import scala.compiletime.constValue

sealed trait Relation0:
  type Schema <: RecordSchema
  val schema: Schema
  type Values = schema.Values
  val values: List[Values]

  sealed trait WithFk:
    type FK <: ForeignKeyId0
    val fk: FK
    transparent inline def join[R2 <: Relation0](inline r2: R2) = 
      JointSchema.join[schema.type, r2.Schema](schema, r2.schema).leftInnerJoin[FK](fk)(values, r2.values)
      
  transparent inline def withFk[FK <: ForeignKeyId0](inline fk1: FK) = new WithFk{
    type FK = fk1.type
    val fk = fk1 
  }

object Relation0:
  transparent inline def apply[S <: RecordSchema](inline s: S)(inline data: List[s.Values]) =
    new Relation0 {
      type Schema = s.type
      val schema = s
      val values = data
    }
