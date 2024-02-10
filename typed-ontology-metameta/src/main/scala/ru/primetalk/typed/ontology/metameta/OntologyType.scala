package ru.primetalk.typed.ontology.metameta

import scala.language.implicitConversions

/**
  * Иерархия типов, поддерживаемых онтологией.
  * 
  * Экземпляры этого типа описывают какой-то другой тип с точки зрения онтологии.
  * 
  * Запись/Record - тип, к которому могут быть привязаны значения свойств.
  * Вполне соответствует обычному объекту, или строчке в таблице БД.
  * 
  * Скаляр/Scalar - обыкновенный тип, не имеющий внутренней структуры.
  * Может только иметь значение.
  * 
  * Коллекция/MetaSeq - обыкновенная коллекция наподобие Seq/List/Vector.
  * Отличие от Scalar[Seq[T]] заключается в том, что на мета-уровне можно 
  * будет привязать дополнительные ограничения на количество элементов.
  * 
  * Тип суммы/Sum- к нему можно привязать в онтологии 
  * исчерпывающий набор вариантов. Примерно соответствует sealed trait.
  * 
  * Тип произведения/Product - к ним можно привязать в онтологии 
  * жёсткий перечень вложенных элементов. Примерно соответствует Tuple.
  */
sealed trait OntologyType// TODO: добавить описываемый тип [T]

object OntologyType:
  /** Phantom type that represents a record. */
  abstract final class Record[+A] extends OntologyType
  /** A type that represents a simple Scala type that is supported. */
  abstract final class Scalar[A] extends OntologyType

  /** Phantom type that represents a collection of elements of type A. */
  abstract final class MetaSeq[+A <: OntologyType] extends OntologyType

  abstract final class OntologyEnum[A] extends OntologyType

  abstract final class Sum[A] extends OntologyType
  abstract final class Product[A] extends OntologyType

  given scalarRtti[T](using tprov: RttiProvider[T]): RttiProvider[Scalar[T]] = new RttiProvider[Scalar[T]]:
    def rtti: RuntimeTypeInformation = tprov.rtti

  given metaSeqRtti[T <: OntologyType](using tprov: RttiProvider[T]): RttiProvider[MetaSeq[T]] = new RttiProvider[MetaSeq[T]]:
    def rtti: RuntimeTypeInformation = RuntimeTypeInformation.SeqType(tprov.rtti)

end OntologyType
