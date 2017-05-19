package ru.primetalk.typed.ontology.metameta

import scala.language.higherKinds

/**
  * Package `meta` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
object SimplePropertiesMeta extends Meta {

  type PropertyIdImpl[-A,B] = PropertyId[A,B]
  /**
    * Metainformation about property.
    * Contains unique name (within the type) and type of the value.
    * Might contain other metainformation about property, like Schema.
    */
  case class PropertyId[-A,B](name: String)

  implicit def propertyIdTypeClassInstance: PropertyIdTypeClass[PropertyIdImpl] = PropertyIdTypeClassInstance

  object PropertyId {
    implicit def propertyIdTypeClassInstance = PropertyIdTypeClassInstance
  }
  object PropertyIdTypeClassInstance extends PropertyIdTypeClass[PropertyId] {
    override def name[A, B](p: PropertyId[A, B]): String = p.name
  }

  def property[B](implicit name: sourcecode.Name, r: RecordTypeId): PropertyId[Record[r.RecordType],B] =
    PropertyId(name.value)

  /** Uses TypeTag to capture type information. */
  object TypeTagRtti extends RunTimeTypeInformation {

    import scala.reflect.runtime.universe.TypeTag

    override type RunTimeTypeInfo[A,B,D] = TypeTag[D]

    override type PropertyIdImpl[-A,B] = PropertyId[A,B]

    implicit def rt[A,B,D](implicit tt: TypeTag[D], tm: TypeMapping[B,D]): RunTimeTypeInfo[A,B,D] = tt

  }


}
