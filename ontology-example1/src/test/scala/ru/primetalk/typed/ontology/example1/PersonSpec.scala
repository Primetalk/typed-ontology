package ru.primetalk.typed.ontology.example1

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta._
import ru.primetalk.typed.ontology.Record

class PersonSpec:

  abstract final class Person

  object personProps extends RecordSchemaBuilder[Person]:
    val name = property[String]("name")
    val age = property[Int]("age")// : SimplePropertyId[Record[Person], Int]
    val title = property[String]("title")
    val baseSchema = fields(name, age)
  //     // RecordSchema.empty[Person].prepend(age).prepend(name)// name #: fields(name, age)
  
  @Test def schemaTest =
    println(personProps.baseSchema)
    val bs = personProps.baseSchema
    val person1: bs.Values = (20, "Vasya")
  //   val m1 = bs.convertToMap(person1)

  //   assert(person1.get(personProps.name) == Some("Vasya"))
  //   assert(person1.get(personProps.age) == Some(20))
  //   assert(person1.get(personProps.title) == None)

  //   println(person1)
  //   assert(m1 == Map("name" -> "Vasya", "age" -> 20))
