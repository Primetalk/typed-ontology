package ru.primetalk.typed.ontology.json

import org.json4s.{JsonAST, _}

object JsonConverters {

  implicit val stringConverter = new JsonConverter[String] {
    override def toJson(value: String): JValue = JsonAST.JString(value)

    override def fromJson(jvalue: JValue): String = jvalue match {
      case JsonAST.JString(s) => s
      case _ => throw new IllegalArgumentException(s"Cannot convert $jvalue to String")
    }
  }

  implicit def jObjectRecordConverter[A] = new JsonConverter[JObjectRecord[A]] {
    override def toJson(value: JObjectRecord[A]): JValue = value.jobject

    override def fromJson(jvalue: JValue): JObjectRecord[A] = jvalue match {
      case o:JsonAST.JObject => JObjectRecord[A](o)
      case _ => throw new IllegalArgumentException(s"Cannot convert $jvalue to String")
    }
  }
}
