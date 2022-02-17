package ru.primetalk.typed.ontology.example3

import scala.compiletime.ops.int.{S, +}
import scala.compiletime.{constValue, erasedValue, constValueTuple}

class FibSpec extends BaseSpec:

  type Fibonacci[a<:Int, b<:Int, n<:Int]<:Int = n match
    case 0    => b
    case S[x] => Fibonacci[b, a + b, x]

  transparent inline def fibonacci[n<:Int](inline a: Int, inline b: Int): Int = //Fibonacci[a.type, b.type, n] = 
    inline erasedValue[n] match
      case _: 0    => b
      case _: S[x] => 
        fibonacci[x](b, a + b)

  test("fib"){
    val f3 = fibonacci[3](1,1)
    f3 should equal(5)
    val F3: Fibonacci[1,1,3] = constValue
    F3 should equal(5)
  }
  test("tuple"){
    constValueTuple[(1, "hello")] should equal((1, "hello"))
  }
