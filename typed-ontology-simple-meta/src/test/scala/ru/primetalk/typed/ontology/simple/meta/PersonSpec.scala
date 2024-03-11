package ru.primetalk.typed.ontology.simple.meta

class PersonSpec extends BaseSpec:

  final case class Person(name: String, age: Int) derives SchemaProvider

  test("schema derivation test") {
    println(SchemaProvider[Person].schema)
  }
