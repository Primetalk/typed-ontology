package ru.primetalk.typed.ontology.typeclass.tethys

import tethys.*
import tethys.readers.*
import ru.primetalk.typed.ontology.typeclass.schema.*
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.runtime.Tuples

given reader[Source <: Tuple, SourceV <: Tuple](using
    names: RuntimeNames[Source],
    readers: Tuple.Map[SourceV, JsonReader]
): JsonReader[SourceV] =
  val readersList = readers.toIArray.toList.map(_.asInstanceOf[JsonReader[?]])
  val pairsNameToReader = names.names.zip(readersList)
  val mapNameToReader = pairsNameToReader.toMap

  @tailrec
  def recursiveRead(it: TokenIterator, m: Map[String, Any])(implicit fieldName: FieldName): Map[String, Any] =
    it.currentToken() match 
      case token if token.isObjectEnd => 
        it.nextToken()
        m
      case token if token.isFieldName => 
        val fieldName = it.fieldName()
        val value = reader.asInstanceOf[JsonReader[Any]].read(it)(FieldName(fieldName))
        it.nextToken()
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
  new JsonReader[SourceV]:
    def read(it: TokenIterator)(implicit fieldName: FieldName): SourceV = 
      if (!it.currentToken().isObjectStart)
        ReaderError.wrongJson(
          s"Expected object start but found: ${it.currentToken()}"
        )
      else {
        it.nextToken()
        val extracted: Map[String, Any] = recursiveRead(it, Map())

        val notExtracted: String = collectNotExtracted(
          0,
          hasErrors = false,
          extracted,
          new mutable.StringBuilder()
        )
        if notExtracted.nonEmpty then
          ReaderError.wrongJson(s"Can not extract fields $notExtracted")
        else 
          Tuples.fromArray(names.names.map(extracted).map(i => (i.asInstanceOf[AnyRef]): Object).toArray).asInstanceOf[SourceV]
      }

  // val completeBuilder: JsonReaderBuilder[Any] = pairsNameToReader
  //   .foldLeft(JsonReader.builder: JsonReaderBuilder[Any]) { case (builder, (name, reader)) =>
  //     builder.addField[Any](name)(using reader)
  //   }
  // completeBuilder.buildReader()
