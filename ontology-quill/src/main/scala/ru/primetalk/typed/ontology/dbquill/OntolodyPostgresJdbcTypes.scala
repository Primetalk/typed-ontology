package ru.primetalk.typed.ontology.dbquill

import io.getquill.PostgresJdbcContext
import io.getquill.{NamingStrategy, SnakeCase}
import javax.sql.DataSource
import io.getquill.generic.GenericDecoder
import java.time.LocalDateTime
import ru.primetalk.typed.ontology.simple.meta.{#:, #@}
import java.sql.Timestamp
import org.postgresql.jdbc.TimestampUtils
import io.getquill.generic.GenericEncoder
import java.time.LocalDate
import ru.primetalk.typed.ontology.simple.meta.SchemaLike
import scala.quoted.{Expr, Quotes}
import ru.primetalk.typed.ontology.simple.meta.EmptySchema
import ru.primetalk.typed.ontology.simple.meta.RecordSchema
import ru.primetalk.typed.ontology.simple.meta.SimplePropertyId
import io.getquill.PostgresDialect
import io.getquill.context.jdbc.PostgresJdbcTypes

trait OntolodyPostgresJdbcTypes[+D <: PostgresDialect, +N <: NamingStrategy]
    extends PostgresJdbcTypes[D, N] {

  given annotatedEmptyDecoder: GenericDecoder[
    ResultRow,
    Session,
    EmptyTuple #@ EmptySchema,
    io.getquill.generic.DecodingType.Specific
  ] =
    new:
      def apply(i: Int, rr: ResultRow, s: Session): EmptyTuple #@ EmptySchema =
        EmptyTuple.#@[EmptySchema]

  given annotatedPropDecoder[
      P <: SimplePropertyId[?, V],
      V,
      S <: RecordSchema,
      VS <: Tuple
  ](using
      vdec: GenericDecoder[ResultRow, Session, V, io.getquill.generic.DecodingType.Specific],
      sdec: GenericDecoder[ResultRow, Session, VS #@ S, io.getquill.generic.DecodingType.Specific]
  ): GenericDecoder[
    ResultRow,
    Session,
    (V *: VS) #@ (P #: S),
    io.getquill.generic.DecodingType.Specific
  ] =
    new:
      def apply(i: Int, rr: ResultRow, s: Session): (V *: VS) #@ (P #: S) =
        val v  = vdec(i, rr, s)
        val vs = sdec(i + 1, rr, s)
        (v *: vs).#@[P #: S]

  given annotetedEmptyEncoder: GenericEncoder[EmptyTuple #@ EmptySchema, PrepareRow, Session] =
    new:
      def apply(
          i: Int,
          t: EmptyTuple #@ EmptySchema,
          row: PrepareRow,
          session: Session
      ): PrepareRow =
        row

  given annotatedPropEncoder[
      P <: SimplePropertyId[?, V],
      V,
      S <: RecordSchema,
      VS <: Tuple
  ](using
      venc: GenericEncoder[V, PrepareRow, Session],
      senc: GenericEncoder[VS #@ S, PrepareRow, Session]
  ): GenericEncoder[(V *: VS) #@ (P #: S), PrepareRow, Session] =
    new:
      def apply(i: Int, vvs: (V *: VS) #@ (P #: S), row: PrepareRow, session: Session): PrepareRow =
        vvs match
          case h *: t =>
            val row2 = venc(i, h, row, session)
            senc(i + 1, t.#@[S], row2, session)

}
