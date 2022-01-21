package ru.primetalk.typed.ontology.example3

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta3._
import ru.primetalk.typed.ontology.Record
import compiletime.constValue
import compiletime.ops.int._
import javax.swing.JTree.EmptySelectionModel
import scala.quoted._

def exampleImpl[S1 <: RecordSchema](s1: Expr[S1])(using ts1: Type[S1])(using Quotes):Expr[String] =
  import quotes.reflect._
  // val tpeS1Values = Select(s1.asTerm, Symbol.classSymbol(RecordSchema.getClass.getName).typeMember("Values")
  // val types = ts1..typeMembers// Symbol.classSymbol(RecordSchema.getClass.getName).typeMembers//("Values")
  // val tpeS1Values = types.map((s1.asTerm,  Symbol.classSymbol(RecordSchema.getClass.getName).typeMembers//("Values")
  // Expr(s"tpeS1Values\n${types.map(_.toString).mkString("\n")}")
  // val es1 = s1.asTerm.tpe.typeSymbol.typeMembers// TypeTree.of[S1]
  // Expr(s"es2=$es1")
  val ts1tree = TypeTree.of[S1]
  
  println(s"ts1tree=${ts1tree.show}")
  println(s"ts1tree2=${ts1tree.tpe.typeSymbol.tree.show}")
  println(s"s1=${s1.show}")
  println(s"s1=${s1.asTerm}")
  println(s"t(s1)=${Singleton.apply(s1.asTerm).show}")
  println(s"s1=${Singleton.apply(s1.asTerm).tpe}")
  println(s"s1.si=${Singleton.apply(s1.asTerm).tpe.isSingleton}")
  
  val types = ts1tree.tpe.typeSymbol.typeMembers
  Expr(s"tpeS1Values\n${types.map(_.toString).mkString("\n")}")