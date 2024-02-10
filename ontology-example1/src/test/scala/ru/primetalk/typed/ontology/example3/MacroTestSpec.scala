package ru.primetalk.typed.ontology.example3

import org.junit.Test
import ru.primetalk.typed.ontology.simplemeta3._
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import compiletime.constValue
import compiletime.ops.int._
import javax.swing.JTree.EmptySelectionModel
import scala.quoted._

class MacroTestSpec:
  import RecordSchema.#:

  object Product extends TableBuilder:
    object id   extends column[Int]
    object name extends column[String]
    type TableSchema = id.type #: name.type #: EmptySchema
    val tableSchema = fields2(id, name)

  @Test def constructTest: Unit =
    val res = example1[Product.tableSchema.type](Product.tableSchema)
    println(res)
    // assert(res == "", s"r=$res")

  transparent inline def example1[S1 <: RecordSchema](inline s1: S1) = ${ exampleImpl[S1]('s1) }
