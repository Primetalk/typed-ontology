package ru.primetalk.typed.ontology.example1

import org.junit.Test
import ru.primetalk.typed.ontology.simple.meta._
import ru.primetalk.typed.ontology.metameta.Record
import compiletime.constValue
import compiletime.ops.int._

class MacroSpec:
  type IsZero[T <: Int] <: Boolean = T match 
    case 0 => true
    case S[_] => false

  inline def toInt[T <: Int]: Int = constValue[T]
    // constValue[T] match {
    //   case 0 => 0
    //   // case 1 => 1
    //   case _: S[x] => 1 + toInt[x]
    // }


  @Test def macroTest =
    assert(sum(1,2) == 3)

  @Test def constTest =
    assert(toInt[2] == 2)
  @Test def macro2Test =
    assert(sum(1L,2L) == 3)

  type IndexOfTypeInTupleAux[T<:Tuple, A, N <: Int] <: Int = T match
    case EmptyTuple => Nothing
    case A *: t => N
    case _ *: t => IndexOfTypeInTupleAux[t, A, S[N]]

  type IndexOfTypeInTuple[T<:Tuple, A] = IndexOfTypeInTupleAux[T, A, 0]

  @Test def typeTest =
    type Example = (Int, String, Boolean)
    val example = (1, "", true)
    val inti: IndexOfTypeInTuple[Example, Int] = 0
    val stri: IndexOfTypeInTuple[Example, String] = 1

    val s: String = example(constValue[IndexOfTypeInTuple[Example, String]])
    assert(s == "")
    val b: Boolean = example(constValue[IndexOfTypeInTuple[Example, Boolean]])
    assert(b)
