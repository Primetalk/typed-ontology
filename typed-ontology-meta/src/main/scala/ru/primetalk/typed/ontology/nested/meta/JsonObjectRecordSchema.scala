package ru.primetalk.typed.ontology.nested.meta

sealed trait JsonTypeInfo

case object JsonNumber extends JsonTypeInfo
case object JsonString extends JsonTypeInfo
case object JsonBoolean extends JsonTypeInfo

case class JsonArray(elem: JsonTypeInfo) extends JsonTypeInfo

sealed trait AbstractJsonObject extends JsonTypeInfo:
  def attributes: Map[String, JsonTypeInfo]

case class JsonObject(attributes: Map[String, JsonTypeInfo]) extends AbstractJsonObject

/** This class encapsulates metainformation about an entity (json object) with attributes.
 * 
 */
// case class JsonObjectTypeInfo[T](ontAttributes: List[RecordProperty[T]]) extends JsonObject
