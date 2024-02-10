package ru.primetalk.typed.ontology.nested.meta

import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult
import ru.primetalk.typed.ontology.metameta.RuntimeTypeInformation.PlainType.given
import ru.primetalk.typed.ontology.metameta.OntologyType.scalarRtti

class OntExampleSpec extends BaseSpec:

  object Person extends SelfSchemaBuilder:
    object name extends scalarColumn[String]
    object age extends scalarColumn[Int]
    object title extends scalarColumn[String]
    val namedType = namedEntityType(name, age, title)

  "simple example" should "compile" in {
    Person.name.name should equal ("name")
  }
