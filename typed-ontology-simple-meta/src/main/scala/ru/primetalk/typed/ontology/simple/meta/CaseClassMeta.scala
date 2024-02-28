package ru.primetalk.typed.ontology.simple.meta

import scala.deriving.Mirror
import scala.quoted.*
import scala.compiletime.*

/** Этот объект содержит имя case class'а и имена полей.
  *
  * @param fields
  */
final case class CaseClassMeta[T](className: String, fields: IArray[String])

object CaseClassMeta:
  inline given [T <: Product](using m: Mirror.ProductOf[T]): CaseClassMeta[T] =
    CaseClassMeta(
      className = constValue[m.MirroredLabel],
      fields = constValueTuple[m.MirroredElemLabels].toIArray.map(_.toString())
    )
end CaseClassMeta
