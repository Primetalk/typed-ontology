package ru.primetalk.typed.ontology

import scala.language.{higherKinds, implicitConversions}

/**
  * Meta-meta package contains instruments to define meta level of ontology.
  *
  * For instance here we define type classes that allows us to use arbitrary types as property identifiers.
  */
package object metameta extends TypeMappings {
  /** Phantom type that represents a record. */
  abstract final class Record[A]

  /** Phantom type that represents a collection of elements of type A. */
  abstract final class MetaSeq[+A]

  trait PropertyIdTypeClass[PropertyId[-_,_]]{
    def name[A,B](p: PropertyId[A,B]): String
  }

  trait RecordTypeClass[RecordImpl[_], PropertyIdImpl[-_,_]] { inst =>
//    type PropertyId[-A,B] = PropertyIdImpl[A,B]
    /** Specific handler of record properties of type B mapped to type D.
      * Handler might be made specific to a certain entity type A (do we really want this? Or is it enough to have B,D?)
      */
    trait PropertyHelper[A,B,D] {

    }
    type PropertyHelperImpl[A,B,D]

    trait RecordWrapper[A] {
      def record: RecordImpl[A]
      type Key[B] = PropertyIdImpl[Record[A], B]
      def apply[B,D](key: Key[B])(implicit bd: TypeMapping[B, D], helper: PropertyHelperImpl[A,B,D]): D

      def get[B,D](key: Key[B])(implicit bd: TypeMapping[B, D], helper: PropertyHelperImpl[A,B,D]): Option[D]

      def updated[B,D](key: Key[B], value: D)(implicit bd: TypeMapping[B, D], helper: PropertyHelperImpl[A,B,D]): RecordImpl[A]

      def remove[B, D](key: Key[B])(implicit bd: TypeMapping[B, D], helper: PropertyHelperImpl[A,B,D]): RecordImpl[A]
    }

    def apply[A](record: RecordImpl[A]): RecordWrapper[A]

    type PropertyValue[A, B, D]

    trait RecordSchemaBuilderOps[A] {//{
      def empty: RecordImpl[A]
      def record(propValueList: PropertyValue[A, _, _]*): RecordImpl[A]
    }

    val syntax: RecordSyntax

    def schemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]): RecordSchemaBuilderOps[A]

    trait RecordSyntax {

      implicit def schemaBuilderOps[A](schemaBuilder: SchemaBuilder[A]): RecordSchemaBuilderOps[A] =
        inst.schemaBuilderOps(schemaBuilder)

    }

  }

  implicit def toRecordWrapper[RecordImpl[_], PropertyIdImpl[-_,_], A](record: RecordImpl[A])(implicit r: RecordTypeClass[RecordImpl, PropertyIdImpl]): r.RecordWrapper[A] = {
    r(record)
  }
//  implicit class RecordOps[RecordImpl[_], PropertyIdImpl[-_,_], A](record: RecordImpl[A])(implicit val r: RecordTypeClass[RecordImpl, PropertyIdImpl]){
//
//    val wrapper: r.RecordWrapper[A] = r.apply(record)
//
//    def updated[B,D](key: wrapper.Key[B], value: D)(implicit bd: TypeMapping[B, D], helper: r.PropertyHelper[A,B]): RecordImpl[A] =
//      wrapper.updated(key, value)
//
//    def get[B,D](key: wrapper.Key[B])(implicit bd: TypeMapping[B, D], helper: r.PropertyHelper[A,B]): Option[D] =
//      wrapper.get(key)
//
//    def apply[B,D](key: wrapper.Key[B])(implicit bd: TypeMapping[B, D], helper: r.PropertyHelper[A,B]): D =
//      wrapper.apply(key)
//  }

  /** This mechanism is used to get the type of surrounding Schema in `property`.
    *
    * Inside [[SchemaBuilder]] we define single instance [[SchemaBuilder.ThisSchemaRecordTypeId]]
    * of this trait. And that instance binds RecordType with actual type for which we define schema.
    */
  sealed trait RecordTypeId {
    type RecordType
  }

  trait SchemaBuilder[A] {

    implicit object ThisSchemaRecordTypeId extends RecordTypeId {
      type RecordType = A
    }

  }


  trait Meta { m =>

    type PropertyIdImpl[-A, B]

    implicit def propertyIdTypeClassInstance: PropertyIdTypeClass[PropertyIdImpl]
    /** Uses TypeTag to capture type information. */
    object TypeTagRtti extends RunTimeTypeInformation {

      import scala.reflect.runtime.universe.TypeTag

      override type RunTimeTypeInfo[A,B,D] = TypeTag[B]

      override type PropertyIdImpl[-A,B] = m.PropertyIdImpl[A,B]

      implicit def rt[A,B,D](implicit tt: TypeTag[B], tm: TypeMapping[B,D]): RunTimeTypeInfo[A,B,D] = tt

    }

  }


}


