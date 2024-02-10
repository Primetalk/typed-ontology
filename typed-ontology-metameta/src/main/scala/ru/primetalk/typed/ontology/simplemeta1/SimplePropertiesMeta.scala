package ru.primetalk.typed.ontology.simplemeta1

import scala.language.higherKinds
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import scala.reflect.ClassTag
import scala.quoted.*
import scala.annotation.transparentTrait

/** Type of record property identifier. */
sealed trait RecordProperty[-A]:
  type P
  val name: String
/**
  * Metainformation about property.
  * Contains unique name (within the type) and type of the value.
  * Might contain other metainformation about property, like Schema.
  */
abstract class SimplePropertyId[-A,B](val name: String, val tpe: ClassTag[B]) extends RecordProperty[A]:
  type P = B

  def tpeSimpleName = 
    tpe.runtimeClass.getSimpleName
  
  override def toString: String = 
    s"$name: $tpeSimpleName"

class PropertiesBuilder[R]:
  transparent inline def emptySchema = RecordSchema.empty[R]
  /** Create an object that represents the property. */
  transparent inline def property[T: ClassTag](name: String): SimplePropertyId[Record[R], T] = 
    new SimplePropertyId[Record[R], T](name, summon){}
  transparent inline def fields(inline properties: RecordProperty[Record[R]]*): RecordSchema[R] =
    ${ fieldsImpl('{properties}, '{RecordSchema.empty[R]}) }

def fieldsImpl[R](expr: Expr[Seq[RecordProperty[Record[R]]]], 
  schemaExpr: Expr[RecordSchema[R]]
)(using r: Type[R])(using Quotes): Expr[RecordSchema[R]] = 
  expr match
    case Varargs(Seq())  => schemaExpr
    case Varargs(Seq(a, as*))  => 
      '{ ${fieldsImpl(Varargs(as), schemaExpr)}.prepend($a) }

/** 
  * Schema of properties for record R.
  * We can use tuples to represent instances of this schema.
  */
sealed trait RecordSchema[R]:
  type RT = R
  type Rec = Record[R]
  type ParentSchemaOrNothing <: RecordSchema[R]
  type Properties  <: Tuple
  type PropertySet
  type Values      <: Tuple
  // Higher order operation on each value.
  // Could be used to construct more complex data structures. For instance, tuple of options, or tuple of Either[Error, _]
  type HValues[H[_]] <: Tuple

  def parentSchemaOrNothing: ParentSchemaOrNothing
  val properties: Properties

  override def toString: String =
    properties.toIArray.mkString(", ")

  def get[A, P <: SimplePropertyId[Record[R], A]](p: P)(v: Values): Option[A]

  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any]

object RecordSchema:
  def empty[R] = new RecordSchema[R]:
    type ParentSchemaOrNothing = Nothing
    type Properties = EmptyTuple
    type PropertySet = Nothing
    type Values = EmptyTuple
    type HValues[H[_]] = EmptyTuple

    val properties: Properties = EmptyTuple

    def parentSchemaOrNothing: ParentSchemaOrNothing = 
      throw new IllegalStateException("There is no parent schema for empty schema")
    def get[A, P <: SimplePropertyId[Record[R], A]](p: P)(v: Values): Option[A] = None
    def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] = 
      m

extension [R, S <: RecordSchema[R]](schema: S)
  infix def #:[P <: RecordProperty[Record[R]]](p: P) =
    schema.prepend(p)
    
  def prepend[P <: RecordProperty[Record[R]]](p: P) =
    new RecordSchema[R]:
      type ParentSchemaOrNothing = S
      type Properties = P *: schema.Properties
      type PropertySet = schema.PropertySet | P
      type Values = p.P *: schema.Values
      val properties: Properties = p *: schema.properties
      val parentSchemaOrNothing: ParentSchemaOrNothing = schema
      def get[A, P2 <: SimplePropertyId[Record[R], A]](p2: P2)(v: Values): Option[A] = 
        v match
          case head *: tail =>
            if p2 == p then
              Some(head.asInstanceOf[A])
            else
              schema.get(p2)(tail)
      def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] =
        v match
          case head *: tail =>
            schema.convertToMap(tail, m.updated(p.name, head))

extension [D <: Tuple](t: D)
  def get[T, R, P <: SimplePropertyId[Record[R], T]](p: P)(using schema: RecordSchema[R])(using ev: D =:= schema.Values): Option[T] =
    schema.get(p)(ev(t))
/*
  * Package `meta` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */

//   type PropertyIdImpl[-A,B] = PropertyId[A,B]

//   implicit def propertyIdTypeClassInstance: PropertyIdTypeClass[PropertyIdImpl] = PropertyIdTypeClassInstance

//   object PropertyId {
//     implicit def propertyIdTypeClassInstance = PropertyIdTypeClassInstance
//   }
//   object PropertyIdTypeClassInstance extends PropertyIdTypeClass[PropertyId] {
//     override def name[A, B](p: PropertyId[A, B]): String = p.name
//   }

//   def property[B](implicit name: sourcecode.Name, r: RecordTypeId): PropertyId[Record[r.RecordType],B] =
//     PropertyId(name.value)

//   /** Uses TypeTag to capture type information. */
//   object TypeTagRtti extends RunTimeTypeInformation {

//     override type RunTimeTypeInfo[A,B,D] = TypeTag[D]

//     override type PropertyIdImpl[-A,B] = PropertyId[A,B]

//     implicit def rt[A,B,D](implicit tt: TypeTag[D], tm: TypeMapping[B,D]): RunTimeTypeInfo[A,B,D] = tt

//   }

