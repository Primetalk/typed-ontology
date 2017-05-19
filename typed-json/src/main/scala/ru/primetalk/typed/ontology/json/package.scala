package ru.primetalk.typed.ontology

import org.json4s.{JField, JObject, JValue}
import ru.primetalk.typed.ontology.metameta.{AnyTypeMappings, PropertyIdTypeClass, Record, RecordRepresentation, RecordTypeClass, Schema, SimplePropertiesMeta}
import ru.primetalk.typed.ontology.metameta.SimplePropertiesMeta.PropertyId

import scala.language.{higherKinds, implicitConversions}

/**
  * Package `meta` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package object json extends RecordRepresentation {

  val meta = SimplePropertiesMeta

  implicit val recordSupport: RecordTypeClass[RecordImpl, meta.PropertyIdImpl] =
    JObjectRecordTypeClassInstance

  type RecordImpl[A] = JObject
  type JObjectRecord[A] = JObject

  trait JsonConverter[D] {
    def toJson(value: D): JValue
    def fromJson(jvalue: JValue): D
  }

//  case class JObjectRecord[A](jobject: JObject)

  object JObjectRecord extends JObjectRecordTypeClassInstance.Syntax {

    implicit def jObjectRecordTypeClassInstance = JObjectRecordTypeClassInstance

  }

  object JObjectRecordTypeClassInstance extends RecordTypeClass[JObjectRecord, PropertyId] {
    type PropertyHelperImpl[A, B, D] = JsonConverter[D]
    class RecordWrapperImpl[A](val record: JObjectRecord[A]) extends RecordWrapper[A] {
      override def apply[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): D =
        record.obj.collect{
          case (key.name, value) => helper.fromJson(value)
        }.
          head

      override def get[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): Option[D] =
        record.obj.collect{
          case (key.name, value) => helper.fromJson(value)
        }.
          headOption

      override def updated[B, D](key: Key[B], value: D)(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): JObjectRecord[A] =
        JObject((key.name, helper.toJson(value.asInstanceOf[D])) ::
          record.obj.filterNot(_._1 == key.name))

      override def remove[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): JObjectRecord[A] =
        JObject(record.obj.filterNot(_._1 == key.name))
    }
    override def apply[A](record: JObjectRecord[A]): RecordWrapper[A] =
      new RecordWrapperImpl[A](record)

    class SchemaBuilderOps[A](schemaBuilder: Schema[A]) extends RecordSchemaBuilderOps[A] {
      def empty: JObjectRecord[A] = JObject()
      def record(propValueList: PropertyValue[A, _, _]*): JObjectRecord[A] =
        JObject(
          propValueList.collect{
            case PropertyValue(key, Some(value), _, _, jsonConverter) =>
              (key.asInstanceOf[PropertyId[A,_]].name, jsonConverter.toJson(value)):JField
          }.toList
        )

    }

    def schemaBuilderOps[A](schemaBuilder: Schema[A]): RecordSchemaBuilderOps[A] = new SchemaBuilderOps[A](schemaBuilder)

    trait Syntax extends RecordSyntax with PropertyIdSyntax with RecordTypeMappings with RecordWrapperSyntax
    val syntax = new Syntax {

    }

    case class PropertyValue[A, B, D](
      propertyId: PropertyId[Record[A],B],
      value: Option[D],
      bd: metameta.TypeMapping[B, D],
      r: PropertyIdTypeClass[PropertyId],
      jsonConverter: JsonConverter[D]
    )

    trait PropertyIdSyntax {
      implicit def propertyIdOps[A,B](propertyId: PropertyId[Record[A],B])(implicit r: PropertyIdTypeClass[PropertyId]) =
        new PropertyIdOps[A,B](propertyId: PropertyId[Record[A],B])
    }
    class PropertyIdOps[A,B](propertyId: PropertyId[Record[A],B])(implicit val r: PropertyIdTypeClass[PropertyId]) {

      def :=[D](value: D)(implicit bd: metameta.TypeMapping[B, D], jsonConverter: JsonConverter[D]): PropertyValue[A, B, D] =
        PropertyValue[A, B, D](propertyId, Some(value), bd, r, jsonConverter)

      def ?=[D](value: Option[D])(implicit bd: metameta.TypeMapping[B, D], jsonConverter: JsonConverter[D]): PropertyValue[A, B, D] =
        PropertyValue[A, B, D](propertyId, value, bd, r, jsonConverter)
    }

  }

  trait RecordTypeMappings extends AnyTypeMappings {
    implicit def mapRecordToJObjectRecord[A]: metameta.TypeMapping[Record[A], JObjectRecord[A]] =
      metameta.typeMapping[Record[A], JObjectRecord[A]]
//      JObjectRecord.mapRecordToJObjectRecord
  }

}
