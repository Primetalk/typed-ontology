package ru.primetalk.typed.ontology.metameta

import scala.language.higherKinds

/**
  * This trait allows to preserve type information to make it available at runtime.
  */
trait RunTimeTypeInformation {

  type PropertyIdImpl[-A, B]

  type RunTimeTypeInfo[A, B, D]

  case class PropertyTypeInfo[A, B, D](propertyIdImpl: PropertyIdImpl[A, B], runTimeTypeInfo: RunTimeTypeInfo[A, B, D])

  case class PropertiesTypeInfo(propertyTypeInfos: Seq[PropertyTypeInfo[_, _, _]]) {
    def map = propertyTypeInfos.map(p => p.propertyIdImpl -> p).toMap[PropertyIdImpl[_, _], PropertyTypeInfo[_, _, _]]
  }

  implicit def captureTypeInfo[A, B, D](propertyIdImpl: PropertyIdImpl[A, B])(implicit rt: RunTimeTypeInfo[A, B, D]): PropertyTypeInfo[A, B, D] =
    PropertyTypeInfo[A, B, D](propertyIdImpl, rt)

  implicit class PropertyIdImplOps[A, B](propertyIdImpl: PropertyIdImpl[A, B]) {
    def captureTypeInfo[D](implicit rt: RunTimeTypeInfo[A, B, D]): PropertyTypeInfo[A, B, D] =
      PropertyTypeInfo[A, B, D](propertyIdImpl, rt)
  }

  def preserveTypeInfo(propertyTypeInfos: PropertyTypeInfo[_, _, _]*) =
    PropertiesTypeInfo(propertyTypeInfos)
}
