package ru.primetalk.typed.ontology.simplemeta

import scala.reflect.ClassTag
import scala.quoted.Expr
import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.Varargs

trait RecordSchemaBuilderBase:
  type RecordType

trait SchemaBuilder extends RecordSchemaBuilderBase:
  transparent inline def emptySchema = RecordSchema.empty

  transparent inline def fieldsReverse(inline properties: RecordProperty0*): RecordSchema =
    ${ fieldsReverseImpl[EmptySchema]('{properties}, '{RecordSchema.empty}) }

  transparent inline def fields(inline properties: RecordProperty0*): RecordSchema =
    ${ fieldsImpl[EmptySchema]('{properties}, '{RecordSchema.empty}) }

  transparent inline def tupleToSchema[T <: Tuple](inline t: T): RecordSchema.TupleToSchema[T] = 
    ${tupleToSchemaImpl('t)}

  transparent inline def showExpr(inline a: Any): String = 
    ${showExprImpl('a)}

def fieldsReverseImpl[S <: RecordSchema](
  propertyList: Expr[Seq[RecordProperty0]], 
  schemaExpr: Expr[S]
)(using Type[S])(using Quotes): Expr[RecordSchema] =
  propertyList match
    case Varargs(Seq())  => 
      schemaExpr
    case Varargs(Seq('{ $a: ta }, as*))  =>
      val expr = fieldsReverseImpl(Varargs(as), '{ RecordSchema.prepend(${a}, ${schemaExpr})} )
      expr

def fieldsImpl[S <: RecordSchema](
  propertyList: Expr[Seq[RecordProperty0]], 
  schemaExpr: Expr[S]
)(using Type[S])(using Quotes): Expr[RecordSchema] =
  propertyList match
    case Varargs(as)  =>
      fieldsReverseImpl(Varargs(as.reverse), schemaExpr)

def tupleToSchemaImpl[T<:Tuple](t: Expr[T])(using tt: Type[T])(using Quotes): Expr[RecordSchema.TupleToSchema[T]] = 
  t match 
      case '{EmptyTuple} => '{EmptySchema.asInstanceOf[RecordSchema.TupleToSchema[T]]}
      case '{Tuple1($a: RecordProperty0)} => '{($a #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]}
      case '{Tuple2($a: RecordProperty0, $b: RecordProperty0)} => '{($a #: $b #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]}
      case '{Tuple3($a: RecordProperty0, $b: RecordProperty0, $c: RecordProperty0)} => '{($a #: $b #: $c #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]}
      case '{Tuple4($a: RecordProperty0, $b: RecordProperty0, $c: RecordProperty0, $d: RecordProperty0)} => '{($a #: $b #: $c #: $d #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]}
      case '{Tuple5($a: RecordProperty0, $b: RecordProperty0, $c: RecordProperty0, $d: RecordProperty0, $e: RecordProperty0)} => '{($a #: $b #: $c #: $d #: $e #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]}
      case '{Tuple6($a: RecordProperty0, $b: RecordProperty0, $c: RecordProperty0, $d: RecordProperty0, $e: RecordProperty0, $f: RecordProperty0)} => '{($a #: $b #: $c #: $d #: $e #: $f #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]}
      // TODO: add matches to all TupleN
      case '{($a: RecordProperty0) *: ($t2: Tuple)} => '{(${a} #: ${tupleToSchemaImpl(t2)}).asInstanceOf[RecordSchema.TupleToSchema[T]]}

// The following hangs..
def tupleToSchemaImpl2[T<:Tuple](t: Expr[T])(using tt: Type[T])(using Quotes): Expr[RecordSchema.TupleToSchema[T]] = 
  '{
    $t match 
        case EmptyTuple => EmptySchema.asInstanceOf[RecordSchema.TupleToSchema[T]]
        case (a: RecordProperty0, b: RecordProperty0) => 
          (a #: b #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]
        case (a: RecordProperty0, b: RecordProperty0, c: RecordProperty0) => 
          (a #: b #: c #: EmptySchema).asInstanceOf[RecordSchema.TupleToSchema[T]]
        case (a: RecordProperty0) *: (t2: Tuple) => 
          (a #: ${tupleToSchemaImpl('{t2})}).asInstanceOf[RecordSchema.TupleToSchema[T]]
  }

class RecordSchemaBuilder[R] extends PropertiesBuilder with ForeignKeyBuilder with SchemaBuilder:
  type RecordType = R

/** Table builder autodefines RecordType as equal to this.type.
 * This eliminates the need in a separate phantom type.
 */
abstract class TableBuilder extends PropertiesBuilder with ForeignKeyBuilder with SchemaBuilder:
  type RecordType = this.type


  type TableSchema <: RecordSchema
  val tableSchema: TableSchema 
  type Row = tableSchema.Values

  trait Relation1 extends Relation0 {
    type Schema = tableSchema.type
    val schema = tableSchema
  }
  transparent inline def relation0(inline values1: List[Row]) = 
    new Relation1 {
      val values = values1
    }

  transparent inline def relation2[V[_]](inline values1: V[Row]): Relation2Meta[V] = 
    Relation2Meta(tableSchema)(values1)
  