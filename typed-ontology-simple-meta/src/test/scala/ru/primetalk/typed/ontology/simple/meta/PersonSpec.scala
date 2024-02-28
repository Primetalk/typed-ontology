package ru.primetalk.typed.ontology.simple.meta

import org.junit.Test

class PersonSpec:

  final case class Person(name: String, age: Int) derives SchemaProvider

  @Test def schemaTest =
    println(SchemaProvider[Person].schema)
