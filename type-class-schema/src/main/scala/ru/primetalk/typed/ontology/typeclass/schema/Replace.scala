package ru.primetalk.typed.ontology.typeclass.schema

type Replace[S <: Tuple, FROM, TO] <: Tuple = S match {
  case EmptyTuple => EmptyTuple
  case FROM *: t => TO *: t
  case h *: t => h *: Replace[t, FROM, TO]
}

inline def replace[S <: Tuple, FROM <: Singleton, TO <: Singleton](inline s: S, inline from: FROM, inline to: TO): Replace[S, FROM, TO] =
  inline s match {
    case _: EmptyTuple => EmptyTuple // .asInstanceOf[Replace[EmptyTuple, FROM, TO]]
    case (_ *: tail): (FROM *: t) => to *: tail
    case (head *: tail): (h *: t)  => head *: replace(tail, from, to)
}
