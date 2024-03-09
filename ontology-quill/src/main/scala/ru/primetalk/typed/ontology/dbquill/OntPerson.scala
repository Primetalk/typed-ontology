package ru.primetalk.typed.ontology.dbquill


import io.getquill.*
import io.getquill.context.ContextOperation
import ru.primetalk.typed.ontology.simple.meta.SchemaProvider

import ru.primetalk.typed.ontology.dbquill.parser.SchemaBasedParser
import ru.primetalk.typed.ontology.dbquill.parser.MyTestEntity
import ru.primetalk.typed.ontology.dbquill.parser.ontquery
import ru.primetalk.typed.ontology.dbquill.parser.query
import ru.primetalk.typed.ontology.simple.meta.SimpleTypes.{given, *}
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType
import java.time.LocalDateTime

object OntPerson {
  given schemaBasedParser: SchemaBasedParser.type = SchemaBasedParser
  val ctx = new PostgresJdbcContext(SnakeCase, "testPostgresDB")
  import ctx._

  import SchemaBasedParser.svtGenericDecoder
  import SchemaBasedParser.svtGenericEncoder
  
//  val svtOrder = summon[SchemaValueType[Order.TableSchema, (Int, LocalDateTime)]]
  given svtOrder1: SchemaValueType[Order.TableSchema, Order.svt.Value] = Order.svt

  def main(args: Array[String]): Unit = {
    inline def orderQuery = quote {
      // Order.query(using Order.svt)
      // либо для произвольной схемы: 
        ontquery[Order.TableSchema, Order.svt.Value]("order")//(using Order.svt)
    }
    // val orders = run(orderQuery)

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
