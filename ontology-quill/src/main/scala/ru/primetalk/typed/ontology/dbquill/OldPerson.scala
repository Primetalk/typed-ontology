package ru.primetalk.typed.ontology.dbquill

import io.getquill.*
import io.getquill.context.ContextOperation
import ru.primetalk.typed.ontology.simple.meta.SchemaProvider
import ru.primetalk.typed.ontology.dbquill.parser.{CustomOps, CustomParser}
import io.getquill.ast.{Ast, Entity}
import io.getquill.quat.Quat

case class OldPerson(firstName: String, lastName: String, age: Int) derives SchemaProvider

object OldPersonApp {

  val provider     = SchemaProvider[OldPerson]
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
    inline def personQuillSchemaAgePower = quote {
      personQuillSchema.map(p => p.age ** 2)
    }
    inline def somePeople(named: String) = quote {
      personQuillSchema.filter(p => p.firstName == lift(named))
    }
    val people: List[OldPerson] = run(somePeople("Joe"))

    val agePowered: List[Double] = run(personQuillSchemaAgePower)
    println(people)
  }
}
