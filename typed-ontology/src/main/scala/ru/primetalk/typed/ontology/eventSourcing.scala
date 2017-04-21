package ru.primetalk.typed.ontology

import ru.primetalk.typed.ontology.metameta.SimplePropertiesMeta.PropertyId

/**
  * Defines events about entities
  */
object eventSourcing {

  sealed trait Event

  trait AboutRecord

  case class NewPropertyValue[A,B](prop: PropertyId[A,B], value: B) extends Event
}
