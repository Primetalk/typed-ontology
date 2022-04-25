package ru.primetalk.typed.ontology.nested.meta

import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.MatchResult

abstract class BaseSpec extends AnyFlatSpec with should.Matchers 
  with OptionValues with Inside with Inspectors:

  def IsRight[A, B]: BeMatcher[Either[A, B]] = 
    BeMatcher.apply(e => MatchResult.apply(e.isRight, "{} not is right", "{} is right"))
