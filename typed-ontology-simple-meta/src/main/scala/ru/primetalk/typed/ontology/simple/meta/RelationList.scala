/**
  * Package `simplemeta2` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simple.meta

import scala.language.higherKinds
import ru.primetalk.typed.ontology.Record
import scala.quoted.*
import scala.reflect.ClassTag
import scala.compiletime.ops.int.S
import scala.compiletime.constValue
/** A simple version of relation that doesn't depend on cats and keep data in a List.
 */
trait RelationList:
  type Schema <: RecordSchema
  val schema: Schema

  type Values = schema.Values
  val rows: List[Values]

  sealed trait WithFk:
    type FK <: ForeignKeyId0
    val fk: FK
    transparent inline def join[R2 <: RelationList](inline r2: R2) = 
      JointSchema.join[schema.type, r2.Schema](schema, r2.schema).leftInnerJoin[FK](fk)(rows, r2.rows)

  transparent inline def withFk[FK <: ForeignKeyId0](inline fk1: FK) = new WithFk{
    type FK = fk1.type
    val fk = fk1 
  }

  transparent inline def projection[S2 <: RecordSchema](inline s2: S2) =
    val f = s2.projectorFrom(schema)
    val v = rows.map(f)
    new RelationList {
      type Schema = s2.type
      val schema = s2
      val rows = v
    }
    
object RelationList:
  // type Concat[R1 <: RelationList {type Schema}, R2 <: RelationList{type Schema}] = RelationList {
  //   type Schema = RecordSchema.Concat[R1#Schema, R2#Schema]
  // }
  transparent inline def apply[S <: RecordSchema](inline s: S)(inline data: List[s.Values]) =
    new RelationList {
      type Schema = s.type
      val schema = s
      val rows = data
    }

  // type Product[R1 <: RelationList, R2 <: RelationList] = 
  //   R1 match
  //     case 
  //   RelationList {
  //     type Schema = RecordSchema.Concat[R1#Schema, R2#Schema]
  //   }

import RecordSchema.Concat

transparent inline def fullInnerJoin[
    T1 <: RelationList,
    T2 <: RelationList,
    FK <: ForeignKeyId0](
    inline table1: T1, 
    inline table2: T2,
    inline fk: FK
    ): List[Tuple.Concat[table1.Values, table2.Values]] = 
    for
      row1 <- table1.rows
      row2 <- table2.rows
      if table1.schema.get(fk.left)(row1) == table2.schema.get(fk.right)(row2)
    yield
      row1 ++ row2

transparent inline def crossProduct[
    T1 <: RelationList,
    T2 <: RelationList](
    inline table1: T1, 
    inline table2: T2
    ): List[Tuple.Concat[table1.Values, table2.Values]] = 
    for
      row1 <- table1.rows
      row2 <- table2.rows
    yield
      row1 ++ row2
