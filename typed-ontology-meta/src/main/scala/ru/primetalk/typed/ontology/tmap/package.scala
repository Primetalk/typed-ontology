package ru.primetalk.typed.ontology

import ru.primetalk.typed.ontology.metameta.SimplePropertiesMeta.PropertyId
import ru.primetalk.typed.ontology.metameta.{AnyTypeMappings, PropertyIdTypeClass, Record, RecordRepresentation, RecordTypeClass, SchemaBuilder, SimplePropertiesMeta}

import scala.language.{higherKinds, implicitConversions}

/**
  * Created by zhizhelev on 15.04.17.
  */
package object tmap extends RecordRepresentation {

  val meta = SimplePropertiesMeta

  implicit val recordSupport: RecordTypeClass[RecordImpl, meta.PropertyIdImpl] = TypedMapRecordTypeClassInstance

  type RecordImpl[A] = TypedMap[A]

  case class TypedMap[A](map: Map[PropertyId[_,_], _])

  /** This companion object defines implicits for TypedMap.*/
  object TypedMap extends TypedMapRecordTypeClassInstance.RecordSyntax
    with TypedMapRecordTypeClassInstance.PropertyIdSyntax
    with TypedMapRecordTypeClassInstance.PropertyHelperSyntax
    with RecordTypeMappings
  {
    implicit def typedMapRecordTypeClassInstance = TypedMapRecordTypeClassInstance
  }

  object TypedMapRecordTypeClassInstance extends RecordTypeClass[TypedMap, PropertyId] { inst =>

    object NoPropertyHelper

    type PropertyHelperImpl[A, B, D] = NoPropertyHelper.type

    class RecordWrapperImpl[A](val record: TypedMap[A]) extends RecordWrapper[A] {
      override def apply[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): D =
        record.map(key).asInstanceOf[D]

      override def get[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): Option[D] =
        record.map.get(key).asInstanceOf[Option[D]]

      override def updated[B, D](key: Key[B], value: D)(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): TypedMap[A] =
        TypedMap(record.map.updated(key, value))

      override def remove[B, D](key: Key[B])(implicit bd: metameta.TypeMapping[B, D], helper: PropertyHelperImpl[A, B, D]): TypedMap[A] = TypedMap(record.map.filterKeys(_ == key))
    }

    override def apply[A](record: TypedMap[A]): RecordWrapper[A] =
      new RecordWrapperImpl[A](record)

    //type PropertyValue[A, B, D] = tmap.PropertyValue[PropertyId, A, B, D]
    case class PropertyValue[A, B, D](propertyId: PropertyId[Record[A],B], value: Option[D], bd: metameta.TypeMapping[B, D], r: PropertyIdTypeClass[PropertyId])

    class PropertyIdOps[A,B](propertyId: PropertyId[Record[A],B])(implicit val r: PropertyIdTypeClass[PropertyId]) {

      def :=[D](value: D)(implicit bd: metameta.TypeMapping[B, D]): PropertyValue[A, B, D] =
        PropertyValue[A, B, D](propertyId, Some(value), bd, r)

      def ?=[D](value: Option[D])(implicit bd: metameta.TypeMapping[B, D]): PropertyValue[A, B, D] =
        PropertyValue[A, B, D](propertyId, value, bd, r)
    }

    def propertyIdOps[A,B](propertyId: PropertyId[Record[A],B])(implicit r: PropertyIdTypeClass[PropertyId]): PropertyIdOps[A,B] =
      new PropertyIdOps[A,B](propertyId)

    trait PropertyIdSyntax {
      implicit def propertyIdOps[A,B](propertyId: PropertyId[Record[A],B])(implicit r: PropertyIdTypeClass[PropertyId]): PropertyIdOps[A,B] =
        inst.propertyIdOps[A,B](propertyId)
    }

    class SchemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]) extends RecordSchemaBuilderOps[A] {
      def empty: RecordImpl[A] = TypedMap[A](Map())
      def record(propValueList: PropertyValue[A, _, _]*): RecordImpl[A] =
        TypedMap[A](
          propValueList.collect{
            case PropertyValue(key, Some(value), _, _) =>
              (key.asInstanceOf[PropertyId[A,_]], // recovering type information from GADT skolem. No runtime
                value)
          }.toMap)

    }

    def schemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]): RecordSchemaBuilderOps[A] =
      new SchemaBuilderOps[A](schemaBuilder)

    val syntax = new RecordSyntax with PropertyIdSyntax with PropertyHelperSyntax with RecordTypeMappings {
    }
    trait PropertyHelperSyntax {
      implicit def noPropertyHelper[A, B, D]: PropertyHelperImpl[A, B, D] = NoPropertyHelper
    }
  }

  trait RecordTypeMappings extends AnyTypeMappings {
    implicit def mapRecordToTypedMap[A]: metameta.TypeMapping[Record[A], TypedMap[A]] = mapRecordToImpl
  }

}
