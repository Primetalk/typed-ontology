package ru.primetalk.typed.ontology.example1

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta._
import ru.primetalk.typed.ontology.Record

class PersonSpec:

  abstract final class Person

  object personProps extends PropertiesBuilder[Person]:
    val name = property[String]("name")
    val age = property[Int]("age")// : SimplePropertyId[Record[Person], Int]
    val title = property[String]("title")
    val baseSchema = //name ##: (age ##: emptySchema)
      RecordSchema.empty[Person].prepend(age).prepend(name)
  
  @Test def schemaTest =
    println(personProps.baseSchema)
    implicit val bs = personProps.baseSchema
    val person1: bs.Values = ("Vasya", 20)
    val m1 = bs.convertToMap(person1)

    assert(person1.get(personProps.name) == Some("Vasya"))
    assert(person1.get(personProps.age) == Some(20))
    assert(person1.get(personProps.title) == None)

    println(person1)
    assert(m1 == Map("name" -> "Vasya", "age" -> 20))
