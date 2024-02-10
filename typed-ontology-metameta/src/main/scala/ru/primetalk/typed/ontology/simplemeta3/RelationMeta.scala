/**
  * Package `simplemeta2` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simplemeta3

import scala.language.higherKinds
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import scala.quoted.*
import scala.reflect.ClassTag
import scala.compiletime.ops.int.S
import scala.compiletime.constValue
// DONE: fs2-Stream-based relations
// DONE: relational algebra, including projection
// DONE: Table + columns terminology
// DONE: move simplemeta to -meta
trait Relation0:
  type Schema <: RecordSchema
  val schema: Schema

  type Values = schema.Values
  val values: List[Values]

  sealed trait WithFk:
    type FK <: ForeignKeyId0
    val fk: FK
    transparent inline def join[R2 <: Relation0](r2: R2) = 
      JointSchema.join[schema.type, r2.Schema](schema, r2.schema).leftInnerJoin[FK](fk)(values, r2.values)

  transparent inline def withFk[FK <: ForeignKeyId0](fk1: FK) = new WithFk{
    type FK = fk1.type
    val fk = fk1 
  }

  // transparent inline def projection[S2 <: RecordSchema](inline s2: S2): List[s2.Values] =
  //   inline val f = schema.projection(s2)
  //   values.map(f)
    
object Relation0:
  // type Concat[R1 <: Relation0 {type Schema}, R2 <: Relation0{type Schema}] = Relation0 {
  //   type Schema = RecordSchema.Concat[R1#Schema, R2#Schema]
  // }
  transparent inline def apply[S <: RecordSchema](s: S)(inline data: List[s.Values]) =
    new Relation0 {
      type Schema = s.type
      val schema = s
      val values = data
    }

import RecordSchema.Concat

transparent inline def leftInnerJoin2[
    T1 <: Relation0,
    T2 <: Relation0,
    FK <: ForeignKeyId0](
    table1: T1, 
    table2: T2,
    inline fk: FK
    ): List[Tuple.Concat[table1.Values, table2.Values]] = 
    for
      row1 <- table1.values
      row2 <- table2.values
      if table1.schema.get(fk.left)(row1) == table2.schema.get(fk.right)(row2)
    yield
      row1 ++ row2
