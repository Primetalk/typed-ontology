package ru.primetalk.source.text


import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

abstract class BaseSpec extends AnyFlatSpec with should.Matchers 
  with OptionValues with Inside with Inspectors