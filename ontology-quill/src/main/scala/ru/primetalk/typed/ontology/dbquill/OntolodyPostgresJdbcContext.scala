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

class OntolodyPostgresJdbcContext(dataSource: DataSource)
    extends PostgresJdbcContext(SnakeCase, dataSource)
    with OntolodyPostgresJdbcTypes[PostgresDialect, SnakeCase]
