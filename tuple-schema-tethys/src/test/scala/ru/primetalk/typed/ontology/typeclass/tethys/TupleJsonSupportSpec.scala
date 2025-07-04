package ru.primetalk.typed.ontology.typeclass.tethys

import _root_.tethys.{given, *}
import _root_.tethys.jackson.{given, *}

import ru.primetalk.typed.ontology.typeclass.schema.{RuntimeNames, ColumnName, ValueWithSchema}
import scala.language.experimental.namedTuples
import _root_.tethys.{JsonReader, JsonWriter}

class TupleJsonSupportSpec extends BaseSpec:

  // Test data
  val address1: Address.Row = ("My street", 10)
  val person1: Person.Row   = ("Ivan", address1)

  test("Address JSON serialization roundtrip") {
    // Serialize to JSON
    val json = address1.asJson
    json should include("\"street\":\"My street\"")
    json should be("{\"street\":\"My street\",\"building\":10}")
    // Deserialize back from JSON
    val result: Either[tethys.readers.ReaderError, Address.Row] = json.jsonAs[Address.Row]
    result should be(Right(address1))
  }

  test("Person with nested Address JSON serialization roundtrip") {
    // Serialize to JSON
    val json = person1.asJson
    // Deserialize back from JSON
    val result = json.jsonAs[Person.Row]
    result should be(Right(person1))
  }

end TupleJsonSupportSpec
