package ru.primetalk.typed.ontology.dbquill


import io.getquill.*
import io.getquill.context.ContextOperation
import ru.primetalk.typed.ontology.simple.meta.SchemaProvider

import ru.primetalk.typed.ontology.dbquill.parser.SchemaBasedParser
import ru.primetalk.typed.ontology.dbquill.parser.MyTestEntity
import ru.primetalk.typed.ontology.dbquill.parser.ontquery
import ru.primetalk.typed.ontology.dbquill.parser.quillQuery
import ru.primetalk.typed.ontology.simple.meta.SimpleTypes.{given, *}
import ru.primetalk.typed.ontology.simple.meta.{#@, annotated, SchemaValueType}
import java.time.LocalDateTime
import io.getquill.ast.Entity
import io.getquill.quat.Quat
import scala.collection.mutable.LinkedHashMap
import io.getquill.ast.Renameable
import scala.quoted.Type
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType
import ru.primetalk.typed.ontology.dbquill.parser.TupleConverter

object OntPerson {
  given schemaBasedParser: SchemaBasedParser.type = SchemaBasedParser
  val ctx = new PostgresJdbcContext(SnakeCase, "testPostgresDB")
  import ctx._

  import SchemaBasedParser.svtGenericDecoder
  import SchemaBasedParser.svtGenericEncoder
  
//  val svtOrder = summon[SchemaValueType[Order.TableSchema, (Int, LocalDateTime)]]
  given svtOrder1: SchemaValueType[Order.TableSchema, Order.svt.Value] = Order.svt

  def main(args: Array[String]): Unit = {
    def personQuillSchema = 
      Quoted[OldPerson](
        Entity.Opinionated("person", Nil, 
          Quat.Product.apply(
            "OldPerson", 
            Quat.Product.Type.Concrete, 
            LinkedHashMap("firstName" -> Quat.Value, "lastName" -> Quat.Value, "age" -> Quat.Value), 
            // LinkedHashMap()
          ), 
          Renameable.Fixed
        ), 
        Nil, 
        Nil
      )
// AST=`querySchema`("person")
// AST.quat=OldPerson(firstName:V,lastName:V,age:V)
// lifts=List()
// Entity(person, List(), OldPerson(firstName:V,lastName:V,age:V), renamable = Fixed)
// Quat.Product(OldPerson, LinkedHashMap(firstName -> V, lastName -> V, age -> V), LinkedHashMap(), Concrete)

    println(s"AST=${personQuillSchema.ast}")
    println(s"AST.quat=${personQuillSchema.ast.quat}")
    println(s"lifts=${personQuillSchema.lifts}")
    personQuillSchema.ast match
      case e@Entity(name, properties, quat) =>
        println(s"Entity($name, $properties, $quat, renamable = ${e.renameable})") 
        println(s"Quat.Product(${quat.name}, ${quat.fields}, ${quat.renames}, ${quat.tpe})")
      case _ => 

    inline def orderQuery = quote {
      // Order.query(using Order.svt)
      // либо для произвольной схемы: 
        Order.quillQuery[Order.Row]// тип передаём временно, из-за проблемы, что quill не поддерживает сконструированные Tuple'ы *:
        // ontquery[Order.TableSchema, TupleConverter[Order.svt.Value], Order.svt.AValue]("order")//(using Order.svt)
    }
    // inline given svtGenericDecoder[ResultRow: Type, Session]
    //   : GenericDecoder[ResultRow, Session, Order.Row, DecodingType.Specific] =
    // new:
    //   def apply(i: Int, rr: ResultRow, s: Session): Order.Row =
    //     var res: Order.svt.Value | Null = null
    //     val a             = res.annotated[Order.TableSchema]
    //     res.asInstanceOf[Order.Row]

    val orders = run(orderQuery)

    // println(run(MyTestEntityQuery).string)
    // val make = ContextOperation.Factory[ctx.idiom.type, ctx.naming.type, PrepareRow, ResultRow, Session, ctx.type](ctx.idiom, ctx.naming)
    // val o = make.op[Nothing, OldPerson, Result[RunQueryResult[OldPerson]]]

    // o.apply { arg =>
    //   println(arg.sql)
    //   ???
    // }
//    summon[ToSql[EntityQuery[Person]]].
    //println(somePeople("Joe")) // TODO Get SQL
    // println(orders)
  }
}
