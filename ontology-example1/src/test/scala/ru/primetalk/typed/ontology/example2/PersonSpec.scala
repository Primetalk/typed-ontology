package ru.primetalk.typed.ontology.example2

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta2._
import ru.primetalk.typed.ontology.metameta.Record
import compiletime.ops.int._

class PersonSpec:


  abstract final class Person

  object personProps extends RecordSchemaBuilder[Person]:
    object name extends SimplePropertyId[Person, String]("name", summon)
    object age extends SimplePropertyId[Person, Int]("age", summon)
    object title extends SimplePropertyId[Person, String]("title", summon)
    // val age = property[Int]("age")// : SimplePropertyId[Record[Person], Int]
    // val title = property[String]("title")
    // val nameSchema: SchemaCons[name.type, EmptySchema] = fields[name.type](name)
    // val nameSchema: SchemaCons[name.type, EmptySchema] = 
    //   RecordSchema.empty.append(name)
    val baseSchema = fields2(name, age)//RecordSchema.empty.prepend(age).prepend(name)// name #: 
    val schema2 = fields(name, age)
    // val schema2 = tupleToSchema((name, age))
  
  @Test def schemaTest =
    type UnOption[O <: Option[?], P] <: Any = O match
      case Option[P] => Int
      case Option[_] => String
      case _ => Double

    // val i: UnOption[Option[String], String] = 1
    // val h: UnOption[Option[String], Int] = ""
    println(personProps.baseSchema)
    // personProps.nameSchema.values(Tuple1(""))
    // val ns: RecordSchema {
    //   type R = Person | Nothing
    // } = personProps.nameSchema
    implicit val bs = personProps.baseSchema
    val person1: bs.Values = bs.values(("Vasya", 20))
    val m1 = bs.convertToMap(person1)
    // val ni = personProps.baseSchema.indexOfProp[personProps.name.type, personProps.baseSchema.type](personProps.name)
    val ni = bs.indexOfProp(personProps.name)
    assert(person1(ni) == "Vasya")
    assert(person1(bs.indexOfProp(personProps.age)) == 20)
    // assert(person1(bs.indexOfProp(personProps.title)) == "Vasya", s"index of title = ${bs.indexOfProp(personProps.title)}")
    assert(bs.get(personProps.name)(person1) == Some("Vasya"))
    assert(bs.get(personProps.age)(person1) == Some(20))
    assert(bs.get(personProps.title)(person1) == None)
    val h: personProps.schema2.Values = (1, "")
    // val h: personProps.schema2.Values = (1, "", 1)
    println(person1)
    assert(m1 == Map("name" -> "Vasya", "age" -> 20))
