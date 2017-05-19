package ru.primetalk.typed.ontology.metajsoncvt

import ru.primetalk.typed.ontology.json.{JObjectRecord, JObjectRecordTypeClassInstance}
import ru.primetalk.typed.ontology.metameta.SimplePropertiesMeta.PropertyId
import ru.primetalk.typed.ontology.metameta.{Record, TypeMapping}
import ru.primetalk.typed.ontology.tmap.{TypedMap, TypedMapRecordTypeClassInstance}

/**
  * This object allows to convert tmaps to json representation
  * using captured runtime type information.
  */
case class ConvertTmapJson1[A,B,D](propertyId: PropertyId[Record[A],B])(implicit typeMapping: TypeMapping[B,D],
  phTM: TypedMapRecordTypeClassInstance.PropertyHelper[A,B,D], ph2: JObjectRecordTypeClassInstance.PropertyHelper[A,B,D]) {

  def convert(tmap: TypedMap[A]): JObjectRecord[A] = {
    //    JObjectRecordTypeClassInstance.
    val valueOpt = ??? //TypedMapRecordTypeClassInstance.apply(tmap).get(propertyId)(typeMapping, phTM)
    ???
  }
}
/**
  * This object allows to convert tmaps to json representation
  * using captured runtime type information.
  */
case class ConvertTmapJson(tmapRtti: Any, jsonRtti: Any) {

  def convert[A](tmap: TypedMap[A]): JObjectRecord[A] =
    ??? //tmap.get(pe)
}
