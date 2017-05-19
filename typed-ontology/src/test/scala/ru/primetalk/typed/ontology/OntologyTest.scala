package ru.primetalk.typed.ontology

import org.scalatest.FunSuite

/**
  * Test of various ways of handling data with typed ontology.
  */
class OntologyTest extends FunSuite {
  import ru.primetalk.typed.ontology.ontology.{address, person}

  test("Creating complex records and reading values"){
    import tmap.TypedMap._

    val alice = person.record(
      person.name := "Alice",
      person.address := address.record(
        address.postalIndex := "12345",
        address.street := "Blueberry street, 8"
      )
    )
    assertResult(Some("Alice"))(alice.get(person.name))
    assertResult("12345")(alice(person.address).apply(address.postalIndex))
  }

  test("Creating records and reading values"){
    import tmap.TypedMap._
    val alice = person.empty.updated(person.name, "Alice")
    assertResult(Some("Alice"))(alice.get(person.name))
  }

  trait AliceData {
    import ru.primetalk.typed.ontology.metameta.toRecordWrapper
    import ru.primetalk.typed.ontology.metameta.RecordRepresentation
    val meta: RecordRepresentation
    import meta.RecordImpl
//    def alice = person.record(
//      person.name := "Alice",
//      person.address := address.record(
//        address.postalIndex := "12345",
//        address.street := "Blueberry street, 8"
//      )
//    )
  }

  trait AliceDataCheck {
    import ru.primetalk.typed.ontology.metameta.toRecordWrapper
    import ru.primetalk.typed.ontology.metameta.RecordRepresentation
    val meta: RecordRepresentation
    import meta.RecordImpl
    import meta.recordSupport
//    def checkAlice(alice: RecordImpl[ontology.Person]) = {
//      assertResult(Some("Alice"))(alice.get(person.name))
//      assertResult("12345")(alice(person.address).apply(address.postalIndex))
//    }
  }

  test("Test data with another data representation"){
    import json.JsonConverters._
    import json.JObjectRecord._
    import json.JObjectRecordTypeClassInstance


    def alice = person.record(
      person.name := "Alice",
      person.address := address.record(
        address.postalIndex := "12345",
        address.street := "Blueberry street, 8"
      )
    )
    val alice1 = alice
    assertResult(Some("Alice"))(JObjectRecordTypeClassInstance(alice1).get(person.name))
    assertResult("12345")(toRecordWrapper(toRecordWrapper(alice1).apply(person.address)).apply(address.postalIndex))

  }
}
