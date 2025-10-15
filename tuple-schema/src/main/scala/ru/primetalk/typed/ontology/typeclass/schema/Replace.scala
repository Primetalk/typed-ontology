package ru.primetalk.typed.ontology.typeclass.schema

import scala.compiletime.*

type Replace[S <: Tuple, FROM, TO] <: Tuple = S match {
  case EmptyTuple => EmptyTuple
  case FROM *: t => TO *: t
  case h *: t => h *: Replace[t, FROM, TO]
}

inline def replace[S <: Tuple, FROM <: Singleton, TO <: Singleton](s: S, from: FROM, to: TO): Replace[S, FROM, TO] =
  inline erasedValue[S] match {
    case _: EmptyTuple => EmptyTuple
    case _: (FROM *: t) =>
      to *: s.asInstanceOf[FROM *: t].tail
    case _: (h *: t)  =>
      val tup = s.asInstanceOf[h *: t]
      tup.head *: replace[t, FROM, TO](tup.tail, from, to)
  }
  