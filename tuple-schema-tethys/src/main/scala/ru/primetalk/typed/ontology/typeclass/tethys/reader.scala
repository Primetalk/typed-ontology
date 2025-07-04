package ru.primetalk.typed.ontology.typeclass.tethys

import tethys.*
import tethys.commons.LowPriorityInstance
import tethys.readers.*
import ru.primetalk.typed.ontology.typeclass.schema.*
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.runtime.Tuples

given lowPriorityReader[S, V](using
    reader: JsonReader[V]
): LowPriorityInstance[JsonReader[ValueWithSchema[S, V]]] =
  LowPriorityInstance(
    new:
      def read(it: TokenIterator)(implicit fieldName: FieldName): ValueWithSchema[S, V] =
        reader.read(it)
  )

given readerForRecordValueTuple[Source <: Tuple, SourceV <: Tuple](using
    names: RuntimeNames[Source],
    readers: Tuple.Map[SourceV, JsonReader]
): JsonReader[ValueWithSchema[Source, SourceV]] =
  val size              = names.names.length
  val readersList       = readers.toIArray.toList.map(_.asInstanceOf[JsonReader[?]])
  val pairsNameToReader = names.names.zip(readersList)
  val mapNameToReader: Map[String, JsonReader[?]] = pairsNameToReader.toMap

  require(size == mapNameToReader.size)
  @tailrec
  def recursiveRead(it: TokenIterator, m: Map[String, Any])(using
      fieldName: FieldName
  ): Map[String, Any] =
    it.currentToken() match
      case token if token.isObjectEnd =>
        it.nextToken()
        m
      case token if token.isFieldName =>
        val fieldName: String = it.fieldName()
        it.nextToken()
        val fieldReader: JsonReader[?] = mapNameToReader.getOrElse(
          fieldName,
          ReaderError.wrongJson(
            s"No JsonReader found for field '$fieldName'. There are the following field names: ${mapNameToReader.keys}. Names are: ${names.names}"
          )
        )
        val value = fieldReader.asInstanceOf[JsonReader[Any]].read(it)(using FieldName(fieldName))
        recursiveRead(it, m + (fieldName -> value))
      case token =>
        ReaderError.wrongJson(
          s"Expect end of object or field name but '$token' found"
        )

  @scala.annotation.tailrec
  def collectNotExtracted(
      i: Int,
      hasErrors: Boolean,
      extracted: Map[String, Any],
      builder: mutable.StringBuilder
  ): String =
    if (i >= names.names.length) {
      if (hasErrors) builder.toString() else ""
    } else {
      val fieldName = names.names(i)
      if (extracted.contains(fieldName)) {
        collectNotExtracted(i + 1, hasErrors, extracted, builder)
      } else {
        if (hasErrors) builder.append(", ")
        builder.append(fieldName)
        collectNotExtracted(i + 1, hasErrors = true, extracted, builder)
      }
    }
  new JsonReader[ValueWithSchema[Source, SourceV]]:
    def read(it: TokenIterator)(using fieldName: FieldName): ValueWithSchema[Source, SourceV] =
      if (it.currentToken().isObjectStart)
        it.nextToken() // object start
        val extracted: Map[String, Any] = recursiveRead(it, Map())

        val notExtracted: String = collectNotExtracted(
          0,
          hasErrors = false,
          extracted,
          new mutable.StringBuilder()
        )
        if notExtracted.nonEmpty then ReaderError.wrongJson(s"Can not extract fields $notExtracted")
        else
          Tuples
            .fromArray(
              names.names.map(extracted).map(i => (i.asInstanceOf[AnyRef]): Object).toArray
            )
            .asInstanceOf[SourceV]
      else
        ReaderError.wrongJson(
          s"Expected object start but found: ${it.currentToken()}"
        )

  // val completeBuilder: JsonReaderBuilder[Any] = pairsNameToReader
  //   .foldLeft(JsonReader.builder: JsonReaderBuilder[Any]) { case (builder, (name, reader)) =>
  //     builder.addField[Any](name)(using reader)
  //   }
  // completeBuilder.buildReader()
