package ru.primetalk.typed.ontology.typeclass.fschema

import ru.primetalk.typed.ontology.typeclass.schema.ValueWithSchema

import scala.annotation.implicitNotFound

@FunctionalInterface
@implicitNotFound(msg = "Cannot obtain ${Column} of type ${ColumnValue} from ${Value}")
trait GetterF[F[+_], Column, ColumnValue, Value <: Tuple]:
  def apply(rtc: Tuple.Map[Value, F]): F[ValueWithSchema[Column, ColumnValue]]
