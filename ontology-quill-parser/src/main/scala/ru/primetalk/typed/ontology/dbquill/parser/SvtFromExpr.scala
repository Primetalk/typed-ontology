package ru.primetalk.typed.ontology.dbquill.parser

import scala.quoted.*
import ru.primetalk.typed.ontology.simple.meta.SchemaValueType
import ru.primetalk.typed.ontology.simple.meta.SchemaLike
//import quotidian.* 

given svtFromExpr[S<: SchemaLike: Type, V: Type]: FromExpr[SchemaValueType[S, V]] = 
  new FromExpr[SchemaValueType[S, V]]:

    /** Return the value of the expression.
     *
     *  Returns `None` if the expression does not represent a value or possibly contains side effects.
     *  Otherwise returns the `Some` of the value.
     */
    def unapply(expr: Expr[SchemaValueType[S, V]])(using Quotes): Option[SchemaValueType[S, V]] = 
      import quotes.reflect.*
      expr match
        case '{ new SchemaValueType[`S`, `V`] } =>
          Some(new SchemaValueType[S, V])
        case _ => None
