/** Package `simplemeta2` contains definitions that are used to express ontology. For instance, here
  * we define PropertyId class that represent a property description. We might want to put any
  * additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simple.listrelation

import scala.language.higherKinds
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import scala.quoted.*
import scala.reflect.ClassTag
import scala.compiletime.ops.int.S
import scala.compiletime.constValue
import ru.primetalk.typed.ontology.simple.meta.ForeignKeyId0
import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType
import ru.primetalk.typed.ontology.simple.meta.Projector
import ru.primetalk.typed.ontology.simple.meta.RecordSchemaValueType

/** A simple version of relation that doesn't depend on cats and keep data in a List.
  */
trait RelationList:
  type Schema <: RecordSchema

  val schema: Schema
  val svt: SchemaValueType.Aux1[Schema] // = summon[SchemaValueType[schema.type]]

  type Row    = svt.Value
  type Values = Row
  val rows: List[Values]

  sealed trait WithFk:
    type FK <: ForeignKeyId0
    val fk: FK
    // transparent inline def join[R2 <: RelationList](r2: R2) =
    //   val jointSchema: JointSchema[schema.type, r2.schema.type] = JointSchema
    //     .join[schema.type, r2.schema.type](schema, r2.schema)
    //   jointSchema
    //     .leftInnerJoin[FK](fk)(rows, r2.rows)

  transparent inline def withFk[FK <: ForeignKeyId0](fk1: FK) = new WithFk {
    type FK = fk1.type
    val fk = fk1
  }

  transparent inline def projection[S2 <: RecordSchema, VS2](s2: S2)(using
      prj: Projector[Schema, Row, S2, VS2],
      ev: this.Row =:= prj.from.Value,
      ev2: prj.to.type =:= SchemaValueType.Aux1[S2]
  ) =
    val svtS2: SchemaValueType.Aux1[S2] = ev2(prj.to)
    val v                               = rows.map(v => prj.apply(v))
    new RelationList {
      type Schema = S2
      val schema                        = s2
      val svt: SchemaValueType.Aux1[S2] = svtS2
      val rows                          = v.asInstanceOf[List[Values]]
    }

object RelationList:
  transparent inline def apply[S <: RecordSchema](
      s: S
  )(using svt: SchemaValueType.Aux1[S])(inline data: List[svt.Value]) =
    new RelationList {
      type Schema = S
      val schema                            = s
      val svt: SchemaValueType.Aux1[S] = summon[SchemaValueType.Aux1[S]]
      val rows                              = data.asInstanceOf[List[Row]]
    }

import RecordSchema.Concat

// transparent inline def fullInnerJoin[T1 <: RelationList, T2 <: RelationList, FK <: ForeignKeyId0](
//     table1: T1,
//     table2: T2,
//     inline fk: FK
// ): List[Tuple.Concat[table1.Values, table2.Values]] =
//   for
//     row1 <- table1.rows
//     row2 <- table2.rows
//     if table1.schema.get(fk.left)(row1) == table2.schema.get(fk.right)(row2)
//   yield row1 ++ row2

// transparent inline def crossProduct[T1 <: RelationList, T2 <: RelationList](
//     table1: T1,
//     table2: T2
// ): List[Tuple.Concat[table1.Values, table2.Values]] =
//   for
//     row1 <- table1.rows
//     row2 <- table2.rows
//   yield row1 ++ row2
