package ru.primetalk.typed.ontology.metameta

import scala.reflect.ClassTag

/** Описание типа, доступное в runtime.
  */
sealed trait RuntimeTypeInformation // RTTI

trait RttiProvider[T]:
  def rtti: RuntimeTypeInformation

object RttiProvider:
  def apply[T](using RttiProvider[T]): RttiProvider[T] =
    summon[RttiProvider[T]]

  def provide[T](rtti1: RuntimeTypeInformation): RttiProvider[T] = new RttiProvider[T]:
    def rtti: RuntimeTypeInformation = rtti1

object RuntimeTypeInformation:
  def apply[T](using RttiProvider[T]): RuntimeTypeInformation =
    RttiProvider[T].rtti

  val String: ClassTypeInformation  = ClassTypeInformation(summon[ClassTag[String]])
  val Int: ClassTypeInformation     = ClassTypeInformation(summon[ClassTag[Int]])
  val Boolean: ClassTypeInformation = ClassTypeInformation(summon[ClassTag[Boolean]])
  val BigInt: ClassTypeInformation  = ClassTypeInformation(summon[ClassTag[BigInt]])
  val Long: ClassTypeInformation    = ClassTypeInformation(summon[ClassTag[Long]])

  case class ClassTypeInformation(classTag: ClassTag[?]) extends RuntimeTypeInformation:
    override def toString: String = classTag.runtimeClass.getSimpleName

  case class EntityType(columns: Map[String, RuntimeTypeInformation])
      extends RuntimeTypeInformation:
    override def toString: String = columns.map((n, rtti) => s"$n: $rtti").mkString("{", ", ", "}")

  case class EnumValue(name: String, value: Any)
  case class EnumType(values: List[EnumValue]) extends RuntimeTypeInformation:
    override def toString: String = values.map(_.name).mkString("enum(", ", ", ")")

  case class NamedType(name: String, rtti: RuntimeTypeInformation) extends RuntimeTypeInformation:
    override def toString: String = name

  // This is used to define types inside some package/object/module
  case class DependentType(otpe: OntologyType) extends RuntimeTypeInformation

  case class Module(publicDeclarations: List[NamedType]) extends RuntimeTypeInformation

  object PlainType:
    def scalaRtti[T: ClassTag]: RttiProvider[T] = new RttiProvider[T]:
      def rtti: RuntimeTypeInformation = ClassTypeInformation(summon[ClassTag[T]])

    given String: RttiProvider[String]   = scalaRtti[String]
    given Int: RttiProvider[Int]         = scalaRtti[Int]
    given Boolean: RttiProvider[Boolean] = scalaRtti[Boolean]
    given BigInt: RttiProvider[BigInt]   = scalaRtti[BigInt]
    given Long: RttiProvider[Long]       = scalaRtti[Long]

  case class SeqType(element: RuntimeTypeInformation) extends RuntimeTypeInformation:
    override def toString = s"Seq[$element]"

/** This provides information about some named instance. For example, an instance might be of type
  * Module and contain other types.
  */
case class RuntimeInstanceInformation(name: String, rtti: RuntimeTypeInformation)
