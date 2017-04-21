package ru.primetalk.typed.ontology.metajsoncvt

import ru.primetalk.typed.ontology.json.JObjectRecord
import ru.primetalk.typed.ontology.tmap.TypedMap

/**
  * This object allows to convert tmaps to json representation
  * using captured runtime type information.
  */
case class ConvertTmapJson(tmapRtti: Any, jsonRtti: Any) {

  def convert[A](tmap: TypedMap[A]): JObjectRecord[A] = ???
}
