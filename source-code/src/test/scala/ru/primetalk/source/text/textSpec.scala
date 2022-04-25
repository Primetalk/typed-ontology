package ru.primetalk.source.text


import org.scalatest.{OptionValues, Inside, Inspectors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TextSpec extends BaseSpec:
  "text" should "concatenate and render" in {
    val text = Indent(List(
      lift("a ") + lift("b ") + lift("c"),
      lift("a2 ") + lift("b2 ") + lift("c2")
    ))
      
    showText(text) should be(
      """  a b c
        |  a2 b2 c2""".stripMargin)
  }

  "show" should "produce text" in {
    given Show[String] with
      def show(s: String): Text = lift("\"") + s + lift("\"")

    render("hello") should be ("\"hello\"")
  }
