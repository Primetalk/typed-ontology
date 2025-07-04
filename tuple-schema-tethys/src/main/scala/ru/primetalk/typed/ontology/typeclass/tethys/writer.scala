package ru.primetalk.typed.ontology.typeclass.tethys

import tethys.*
import ru.primetalk.typed.ontology.typeclass.schema.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.runtime.Tuples
import Tuple.{Head, Tail, Zip}
import tethys.{JsonWriter, JsonObjectWriter}
import tethys.commons.LowPriorityInstance
import tethys.commons.TokenNode.*
import tethys.writers.tokens.TokenWriter

given emptyTuple: EmptyTuple                                   = EmptyTuple
given nonEmptyTuple[A, T <: Tuple](using a: A, t: T): *:[A, T] = a *: t

given valueWithSchemaWriter[S, V](using jw: JsonWriter[V]): JsonWriter[ValueWithSchema[S, V]] =
  new:
    def write(value: ValueWithSchema[S, V], tokenWriter: TokenWriter): Unit =
      jw.write(value.value, tokenWriter)

type ValuesWithSchemas[Source <: Tuple, SourceV <: Tuple] =
  Tuple.Map[
    Zip[Source, SourceV],
    [T] =>> T match
      case h1 *: h2 *: EmptyTuple =>
        ValueWithSchema[h1, h2]
      case _ => Nothing
  ]

/** JsonWriter for tuples with values, using runtime fieldMap names.
  *
  * @tparam Source
  *   The tuple type representing the schema structure
  * @tparam SourceV
  *   The tuple type representing the actual values
  */
given tupleWriter[Source <: Tuple, SourceV <: Tuple](using
    names: RuntimeNames[Source],
    writers: Tuple.Map[ValuesWithSchemas[Source, SourceV], JsonWriter]
): JsonWriter[ValueWithSchema[Source, SourceV]] =
  val writersList       = writers.toList.map(w => w.asInstanceOf[JsonWriter[?]])
  val pairsNameToWriter = names.names.zip(writersList)
  val mapNameToWriter: scala.collection.Map[String, JsonWriter[?]] = pairsNameToWriter.toMap
  val fieldNames                                                   = names.names

  new JsonObjectWriter[ValueWithSchema[Source, SourceV]]:

    def writeValues(value: ValueWithSchema[Source, SourceV], tokenWriter: TokenWriter): Unit =
      writeFields(value.value, fieldNames, tokenWriter)

    private def writeFields(
        tuple: Tuple,
        restFieldNames: List[String],
        tokenWriter: TokenWriter
    ): Unit =
      tuple match
        case EmptyTuple => // Base case - no more fields
        case head *: tail =>
          restFieldNames match
            case Nil =>
              throw IllegalStateException("Not enough field names")
            case headName :: tailNames =>
              tokenWriter.writeFieldName(headName)
              val fieldWriter: JsonWriter[Any] = mapNameToWriter
                .getOrElse(
                  headName,
                  throw new IllegalArgumentException(s"No TokenWriter for field $headName")
                )
                .asInstanceOf[JsonWriter[Any]]
              fieldWriter.write(head, tokenWriter)
              // Recursively write remaining fields
              writeFields(tail.asInstanceOf[SourceV], tailNames, tokenWriter)
