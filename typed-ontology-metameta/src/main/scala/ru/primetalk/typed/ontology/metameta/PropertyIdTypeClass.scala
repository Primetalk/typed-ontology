package ru.primetalk.typed.ontology.metameta

/** Тайп-класс, позволяющий использовать какой-то пользовательский
 * тип для представления свойств объекта.
 * Пользовательский тип PropertyId должен указывать на какой-либо онтологический тип.
 * Свойства существуют только у Record[_].
*/
trait PropertyIdTypeClass[PropertyId[-_,_ <: OntologyType]]:
  /**
    * Реализация тайп-класса предоставит имя свойства.
    *
    * @param p - идентификатор свойства
    * @return имя свойства
    */
  def name[A, B <: OntologyType](p: PropertyId[A,B]): String
