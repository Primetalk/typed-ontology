package ru.primetalk.typed.ontology.dbquill

import io.getquill.PostgresJdbcContext
import io.getquill.{NamingStrategy, SnakeCase}
import javax.sql.DataSource
import io.getquill.generic.GenericDecoder
import java.time.LocalDateTime
import ru.primetalk.typed.ontology.simple.meta.#@
import java.sql.Timestamp
import org.postgresql.jdbc.TimestampUtils
import io.getquill.generic.GenericEncoder
import java.time.LocalDate

class MyPostgresJdbcContext(dataSource: DataSource)
    extends PostgresJdbcContext(SnakeCase, dataSource) {

  type OrderRow  = (*:[Int, *:[LocalDate, EmptyTuple]])
  type OrderARow = OrderRow #@ Order1.TableSchema
  given orderDecoder
      : GenericDecoder[ResultRow, Session, OrderARow, io.getquill.generic.DecodingType.Specific] =
    new:
      def apply(i: Int, rr: ResultRow, s: Session): OrderARow =
        (
          summon[Decoder[Int]](i, rr, s),
          summon[Decoder[LocalDate]](i + 1, rr, s),
        ).#@[Order1.TableSchema]

  given orderEncoder: GenericEncoder[OrderARow, PrepareRow, Session] =
    new:
      def apply(i: Int, t: OrderARow, row: PrepareRow, session: Session): PrepareRow =
        row

}
