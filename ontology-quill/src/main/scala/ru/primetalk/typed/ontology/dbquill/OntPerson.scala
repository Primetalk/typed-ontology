package ru.primetalk.typed.ontology.dbquill

import io.getquill.*
import io.getquill.context.ContextOperation
import ru.primetalk.typed.ontology.simple.meta.SchemaProvider

import ru.primetalk.typed.ontology.dbquill.parser.SchemaBasedParser
import ru.primetalk.typed.ontology.dbquill.parser.MyTestEntity
import ru.primetalk.typed.ontology.dbquill.parser.ontquery
import ru.primetalk.typed.ontology.dbquill.parser.quillQuery
import ru.primetalk.typed.ontology.simple.meta.AnnotatedTypesContext.{given, *}
import ru.primetalk.typed.ontology.simple.meta.{#@, SchemaValueType}
import java.time.LocalDateTime
import io.getquill.ast.Entity
import io.getquill.quat.Quat
import scala.collection.mutable.LinkedHashMap
import io.getquill.ast.Renameable
import scala.quoted.Type
import io.getquill.generic.GenericDecoder
import io.getquill.generic.DecodingType
import ru.primetalk.typed.ontology.dbquill.parser.TupleConverter
import ru.primetalk.typed.ontology.dbquill.parser.ontQuote
import io.getquill.util.LoadConfig
import ru.primetalk.typed.ontology.simple.meta.Projector

object OntPerson {
  given schemaBasedParser: SchemaBasedParser.type = SchemaBasedParser
  val ds  = JdbcContextConfig(LoadConfig("testPostgresDB")).dataSource
  val ctx = new OntolodyPostgresJdbcContext(ds)
  import ctx.{given, *}

  def main(args: Array[String]): Unit = {
    println(Order1.id1) // Необходимо из-за проблемы порядка инициализации.
    inline def orderQuery = quote {
      ontQuote(Order1)
    }

    val orders = run(orderQuery)

    println(orders.map(_ π Order1.smallerSchema))

  }
}
