package ru.primetalk.typed.ontology.example1

import org.junit.Test
import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record

class PersonSpec:

  abstract final class Person

  object personProps extends RecordSchemaBuilder[Person]:
    val name       = property[String]("name")
    val age        = property[Int]("age") // : SimplePropertyId[Record[Person], Int]
    val title      = property[String]("title")
    val baseSchema = fields(age, name)

  @Test def schemaTest =
    println(personProps.baseSchema)
    val bs                 = personProps.baseSchema
    val person1: bs.Values = (20, "Vasya")
    val m1                 = bs.convertToMap(person1)

    assert(bs.get(personProps.name)(person1) == Some("Vasya"))
    assert(bs.get(personProps.age)(person1) == Some(20))
    assert(bs.get(personProps.title)(person1) == None)

    assert(m1 == Map("name" -> "Vasya", "age" -> 20))
