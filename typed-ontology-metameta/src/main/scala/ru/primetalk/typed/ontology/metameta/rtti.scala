package ru.primetalk.typed.ontology.metameta

import scala.reflect.ClassTag

sealed trait RuntimeTypeInformation // RTTI

trait RttiProvider[T]:
  def rtti: RuntimeTypeInformation

object RttiProvider:
  def apply[T](using RttiProvider[T]): RttiProvider[T] = 
      summon[RttiProvider[T]]

object RuntimeTypeInformation:
  def apply[T](using RttiProvider[T]): RuntimeTypeInformation = 
      RttiProvider[T].rtti

  val String = ClassTypeInformation(summon[ClassTag[String]])
  val Int = ClassTypeInformation(summon[ClassTag[Int]])
  val Boolean = ClassTypeInformation(summon[ClassTag[Boolean]])
  val BigInt = ClassTypeInformation(summon[ClassTag[BigInt]])
  val Long = ClassTypeInformation(summon[ClassTag[Long]])
  
  case class ClassTypeInformation(classTag: ClassTag[_]) extends RuntimeTypeInformation:
    override def toString = classTag.runtimeClass.getSimpleName

  case class EntityType(columns: Map[String, RuntimeTypeInformation]) extends RuntimeTypeInformation:
    override def toString = columns.map((n, rtti) => s"$n: $rtti").mkString("{", ", ", "}")

  case class EnumValue(name: String, value: Any)
  case class EnumType(values: List[EnumValue]) extends RuntimeTypeInformation:
    override def toString = values.map(_.name).mkString("enum(", ", ", ")")

  case class NamedType(name: String, rtti: RuntimeTypeInformation) extends RuntimeTypeInformation:
    override def toString = name

  object PlainType:
    def scalaRtti[T: ClassTag]: RttiProvider[T] = new RttiProvider[T]:
        def rtti = ClassTypeInformation(summon[ClassTag[T]])

    given String: RttiProvider[String] = scalaRtti[String]
    given Int: RttiProvider[Int] = scalaRtti[Int]
    given Boolean: RttiProvider[Boolean] = scalaRtti[Boolean]
    given BigInt: RttiProvider[BigInt] = scalaRtti[BigInt]
    given Long: RttiProvider[Long] = scalaRtti[Long]

  case class SeqType(element: RuntimeTypeInformation) extends RuntimeTypeInformation:
    override def toString = s"Seq[${element}]"
