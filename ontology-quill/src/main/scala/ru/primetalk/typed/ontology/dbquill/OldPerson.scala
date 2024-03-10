package ru.primetalk.typed.ontology.dbquill

import io.getquill.*
import io.getquill.context.ContextOperation
import ru.primetalk.typed.ontology.simple.meta.SchemaProvider
import ru.primetalk.typed.ontology.dbquill.parser.{CustomOps, CustomParser}
import io.getquill.ast.{Ast, Entity}
import io.getquill.quat.Quat

case class OldPerson(firstName: String, lastName: String, age: Int) derives SchemaProvider

object OldPersonApp {

  val provider = SchemaProvider[OldPerson]
  val personSchema = provider.schema
  println(personSchema)

  given customParser: CustomParser.type = CustomParser

  import CustomOps.**
  // SnakeCase turns firstName -> first_name
  val ctx = new PostgresJdbcContext(SnakeCase, "testPostgresDB")
  import ctx._

  def main(args: Array[String]): Unit = {
    inline def personQuillSchema = quote {
      querySchema[OldPerson]("person")
    }
    println(s"AST=${personQuillSchema.ast}")
    println(s"AST.quat=${personQuillSchema.ast.quat}")
    println(s"lifts=${personQuillSchema.lifts}")
    personQuillSchema.ast match
      case e@Entity(name, properties, quat) =>
        println(s"Entity($name, $properties, $quat, renamable = ${e.renameable})") 
        println(s"Quat.Product(${quat.name}, ${quat.fields}, ${quat.renames}, ${quat.tpe})")
    //     val name: String,
    // val fields: mutable.LinkedHashMap[String, Quat],
    // override val renames: mutable.LinkedHashMap[String, String],
    // val tpe: Quat.Product.Type
    //     quat match
    //       case Quat.Product() => 
        
      case _ => 
    inline def personQuillSchemaAgePower = quote {
      personQuillSchema.map(p => p.age ** 2)
    }
    inline def somePeople(named: String) = quote {
      personQuillSchema.filter(p => p.firstName == lift(named))
    }
    val people: List[OldPerson] = run(somePeople("Joe"))

    val agePowered: List[Double] = run(personQuillSchemaAgePower)
    // val make = ContextOperation.Factory[ctx.idiom.type, ctx.naming.type, PrepareRow, ResultRow, Session, ctx.type](ctx.idiom, ctx.naming)
    // val o = make.op[Nothing, OldPerson, Result[RunQueryResult[OldPerson]]]

    // o.apply { arg =>
    //   println(arg.sql)
    //   ???
    // }
//    summon[ToSql[EntityQuery[Person]]].
    //println(somePeople("Joe")) // TODO Get SQL
    println(people)
  }
}
