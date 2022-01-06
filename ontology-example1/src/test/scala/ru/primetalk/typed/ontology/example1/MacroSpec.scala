package ru.primetalk.typed.ontology.example1

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta._
import ru.primetalk.typed.ontology.Record
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

