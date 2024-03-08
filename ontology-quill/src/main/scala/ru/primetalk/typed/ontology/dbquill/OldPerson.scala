package ru.primetalk.typed.ontology.dbquill

class OldPerson {
  
}
import io.getquill.*
import ru.primetalk.typed.ontology.simple.meta.SchemaProvider

object MyApp {
  case class Person(firstName: String, lastName: String, age: Int) derives SchemaProvider

  val provider = SchemaProvider[Person]
  val personSchema = provider.schema
  println(personSchema)
  
  // SnakeCase turns firstName -> first_name
  val ctx = new PostgresJdbcContext(SnakeCase, "testPostgresDB")
  import ctx._

  def main(args: Array[String]): Unit = {
    inline def somePeople(named: String) = quote {
      query[Person].filter(p => p.firstName == lift(named))
    }
    val people: List[Person] = run(somePeople("Joe"))
    
    // TODO Get SQL
    println(people)
  }
}
