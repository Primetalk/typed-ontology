package ru.primetalk.typed.ontology.dbquill

import scala.language.implicitConversions

import io.getquill.Quoted

import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.generic.GenericDecoder
import io.getquill.generic.GenericRowTyper
import io.getquill.generic.GenericColumnResolver
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom}
import scala.collection.mutable.LinkedHashMap
import scala.reflect.ClassTag
import scala.reflect.classTag
import io.getquill.context.mirror.Row
import io.getquill.quote
import io.getquill.query
import io.getquill.context.mirror.MirrorSession
import io.getquill.*
import ru.primetalk.typed.ontology.dbquill.parser.TupleConverter
import ru.primetalk.typed.ontology.dbquill.parser.quillQuery
import ru.primetalk.typed.ontology.dbquill.parser.ontQuote
import ru.primetalk.typed.ontology.simple.meta.#@
import java.time.LocalDateTime
import java.time.LocalDate

class TupleSpec extends Spec {

  val ctx = new MirrorContext[MirrorSqlDialect, Literal](MirrorSqlDialect, Literal)
    with MirrorColumnResolving[MirrorSqlDialect, Literal]
  import ctx.{given, _}

  "simple examples" - {
    val s = MirrorSession.default

    "test tuple type" in {
      val IdentP        = Ident("p", quatOf[OldPerson])
      val OldPersonQuat = quatOf[OldPerson].probit

      inline def q = quote { query[OldPerson].map(p => (p.firstName, p.age)) }
      q.ast mustEqual Map(
        Entity("OldPerson", List(), `OldPersonQuat`),
        IdentP,
        ast.Tuple(List(Property(IdentP, "firstName"), Property(IdentP, "age")))
      )
      val result = ctx.run(q)

      val tupleRow = Row("_1" -> "Joe", "_2" -> 123)
      result.extractor(tupleRow, s) mustEqual ("Joe", 123)
    }

    "test case class type" in {
      inline def q = quote { query[OldPerson] }
      val result   = ctx.run(q)

      val tupleRow = Row("firstName" -> "Joe", "lastName" -> "Dow", "age" -> 123)
      result.extractor(tupleRow, s) mustEqual OldPerson("Joe", "Dow", 123)
    }
    "test tuple compatibility" in {
      type T1 = (String, Int)
      type T2 = String *: Int *: EmptyTuple
      summon[T1 =:= T2]
      val t1: T1  = ("", 0)
      val t2: T2  = t1
      val t11: T1 = t2
      t1._2 mustEqual t2._2
    }
    "test tuple quat" in {
      val q1 = quatOf[(String, Int)]
      type StringInt = String *: Int *: EmptyTuple
      val q2 = quatOf[StringInt]
      val q3 = quatOf[TupleConverter[StringInt]]
      q1 mustEqual q3
      q1 mustNot be(q2)
    }
    "test tuple entity" in {
      val IdentT     = Ident("t", quatOf[(String, Int)])
      val Tuple2Quat = quatOf[(String, Int)].probit
      type PersonT = (String, Int)
      inline def q = quote { query[PersonT].map(t => (t._1, t._2)) }
      q.ast mustEqual Map(
        Entity("Tuple2", List(), `Tuple2Quat`),
        IdentT,
        ast.Tuple(List(Property(IdentT, "_1"), Property(IdentT, "_2")))
      )
      val result = ctx.run(q)

      val tupleRow = Row("_1" -> "Joe", "_2" -> 123)
      result.extractor(tupleRow, s) mustEqual ("Joe", 123)
    }
    "test annotated values" in {
      import ru.primetalk.typed.ontology.simple.meta.AnnotatedTypesContext.{given, *}
      inline def q = ontQuote(Order1)
      val time1    = LocalDate.now()
      given dec: GenericDecoder[
        ResultRow,
        Session,
        Order1.ARow,
        io.getquill.generic.DecodingType.Specific
      ] =
        (i: Int, rr: ResultRow, s: Session) => (rr.apply[Int]("id"), rr.apply[LocalDate]("date")).#@[Order1.TableSchema]
      val IdentT     = Ident("t", quatOf[(String, Int)])
      val Tuple2Quat = quatOf[(String, Int)].probit
      q.ast mustEqual Entity(
        "order1",
        List(
          PropertyAlias(List("_1"), "id"),
          PropertyAlias(List("_2"), "date")
        ),
        `Tuple2Quat`
      )

      val s      = MirrorSession.default
      val result = ctx.run(q)

      val order1Row = Row("type" -> "order1", "id" -> 18, "date" -> time1)
      val order1    = Order1.row(18, time1)
      result.extractor(order1Row, s) mustEqual order1
    }
  }
}
