package ru.primetalk.typed.ontology.example1

import compiletime.constValue
import compiletime.ops.int._
import scala.quoted._

transparent inline def sumInt(inline args: Int*): Int =
  ${sumIntImpl('args)}

def sumIntImpl(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] =
  argsExpr match
    case Varargs(Exprs(values)) => 
      val res = values.sum
      Expr(res)

transparent inline def sum[T](inline args: T*)(using n: Numeric[T]): T =
  ${sumImpl('args, 'n)}

def sumImpl1[T: Type](argsExpr: Expr[Seq[T]], nExpr: Expr[Numeric[T]])(using Quotes): Expr[T] = '{
  ${argsExpr} match
    case Seq() => 
      $nExpr.zero
    case seq => 
      seq.reduce($nExpr.plus)
}
def sumImpl[T: Type](argsExpr: Expr[Seq[T]], nExpr: Expr[Numeric[T]])(using Quotes): Expr[T] = 
  argsExpr match
    case Varargs(values) => 
      '{
        given n: Numeric[T] = ${nExpr}
        ${
          values.foldLeft('{n.zero})((a,b) => '{ n.plus(${a}, ${b}) })
        }
      }
// '{
//   ${argsExpr} match
//     case Seq() => 
//       $nExpr.zero
//     case seq => 
//       seq.reduce($nExpr.plus)
// }
