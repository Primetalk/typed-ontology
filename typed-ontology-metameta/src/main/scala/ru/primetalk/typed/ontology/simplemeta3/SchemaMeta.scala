/**
  * Package `simplemeta2` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simplemeta3

import scala.language.higherKinds
import ru.primetalk.typed.ontology.Record
import scala.quoted.*
import scala.reflect.ClassTag
import scala.compiletime.ops.int.S
import scala.compiletime.constValue

/** 
  * Schema of properties for record R.
  * We can use tuples to represent instances of this schema.
  */
sealed trait RecordSchema: 
  self =>
  import RecordSchema._
  type R // phantom type of entity
  type Rec = Record[R]
  type ParentSchemaOrNothing <: RecordSchema { type R <: self.R }
  type Properties  <: Tuple
  type IndexOfProp[P <: RecordProperty0] = 
    RecordSchema.IndexOfTypeInTuple[Properties, P]

  type PropertySet = Tuple.Union[Properties]
  type Values      = Tuple.Map[Properties, RecordProperty0.PropertyValueType]
  // Higher order operation on each value.
  // Could be used to construct more complex data structures. For instance, tuple of options, or tuple of Either[Error, _]
  type HValues[H[_]] = Tuple.Map[Values, H]

  def parentSchemaOrNothing: ParentSchemaOrNothing
  val properties: Properties

  override def toString: String =
    properties.toIArray.mkString(", ")

  def get[P2 <: RecordProperty0](p2: P2)(v: Values): Option[p2.P]

  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any]

  def isEmpty: Boolean
  def values(v: Values): Values = v

  transparent inline def indexOfProp[P2 <: RecordProperty0, This >: this.type <: RecordSchema](inline p2: P2): IndexOfProp[p2.type]
  
  transparent inline def concat[This >: this.type <: RecordSchema, S2 <: RecordSchema](inline schema2: S2): RecordSchema.Concat[This, schema2.type] =
    inline this match
      case _: EmptySchema => 
        schema2
      case sc: SchemaCons[p, s] => 
        (sc.schema.concat(schema2).prepend(sc.p))


type EmptySchema = EmptySchema.type

case object EmptySchema extends RecordSchema:

  import RecordSchema._
  type R = Nothing
  
  type ParentSchemaOrNothing = Nothing
  type Properties = EmptyTuple
  type Values = EmptyTuple
  val properties: Properties = EmptyTuple

  def parentSchemaOrNothing: ParentSchemaOrNothing = 
    throw new IllegalStateException("There is no parent schema for empty schema")
  def get[P2 <: RecordProperty0](p2: P2)(v: Values): Option[p2.P] = 
    None
  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] = 
    m
  def isEmpty: Boolean = true

  def unapply(e: EmptySchema): true = true

  transparent inline def indexOfProp[P2 <: RecordProperty0, This >: this.type <: RecordSchema](inline p2: P2): IndexOfProp[p2.type] = 
    RecordSchema.indexOfProp(this, p2)


final case class SchemaCons[P <: RecordProperty0, S <: RecordSchema](p: P, schema: S) extends RecordSchema:
  import RecordSchema._
  def indexOfThisProp(p: P): 0 = 0
  type R = p.R | schema.R
  type ParentSchemaOrNothing = schema.type
  type Properties = p.type *: schema.Properties
  val properties: Properties = p *: schema.properties
  def parentSchemaOrNothing: ParentSchemaOrNothing = schema
  def get[P2 <: RecordProperty0](p2: P2)(v: Values): Option[p2.P] = 
    if p2 == p then
      Some(v.head.asInstanceOf[p2.P])
    else
      schema.get(p2)(v.tail)
  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] =
    schema.convertToMap(v.tail, m.updated(p.name, v.head))
  def isEmpty: Boolean = false

  def unapply[This >: this.type <: SchemaCons[P, S]]: Unapply[This] =
    Some((p, schema))
  transparent inline def indexOfProp[P2 <: RecordProperty0, This >: this.type <: RecordSchema](inline p2: P2): IndexOfProp[p2.type] = 
    RecordSchema.indexOfProp(this, p2)
    // 0.asInstanceOf[IndexOfProp[This, p2.type, 0]]//constValue[IndexOfProp[This, p2.type, 0]]

object RecordSchema:
  type IndexOfTypeInTupleAux[T<:Tuple, A, N <: Int] <: Int = T match
    case EmptyTuple => Nothing
    case A *: t => N
    case _ *: t => IndexOfTypeInTupleAux[t, A, S[N]]

  type IndexOfTypeInTuple[T<:Tuple, A] = IndexOfTypeInTupleAux[T, A, 0]

  type #:[P <: RecordProperty0, S <: RecordSchema] = SchemaCons[P, S]

  type Unapply[X <: RecordSchema] = X match
    case EmptySchema => None.type
    case SchemaCons[p, s] => Some[(p,s)]

  type PropAt[X <: RecordSchema, N <: Int] <: RecordProperty0 = X match
    case EmptySchema => Nothing
    case SchemaCons[p, s] => 
      N match
        case 0 => p
        case S[n] => PropAt[s, n]

  type PropByProp[X <: RecordSchema, P <: RecordProperty0] <: RecordProperty0 = X match
    case EmptySchema => Nothing
    case SchemaCons[P, s] => P
    case SchemaCons[_, s] => PropByProp[s, P]

  
  transparent inline def indexOfProp[X <: RecordSchema, P <: RecordProperty0](inline schema: X, inline property: P): schema.IndexOfProp[P] = 
    constValue[schema.IndexOfProp[P]]

  /** Type of the concatenation of two schemas. */
  type Concat[X <: RecordSchema, Y <: RecordSchema] <: RecordSchema = 
    X match 
      case EmptySchema => Y
      case SchemaCons[x1, xs1] => SchemaCons[x1, Concat[xs1, Y]]
    
  // type Concat[S1 <: RecordSchema, S2 <: RecordSchema] = 
  //   S1 match
  //     case EmptySchema => S2
  //     case SchemaCons[p, s] => SchemaCons[p, Concat[s, S2]] 
      

  /** Type of the element at position N in the tuple X */
 
  def empty: EmptySchema = EmptySchema

extension [S <: RecordSchema](schema: S)
  def prepend[P <: RecordProperty0](p: P) =
    SchemaCons[P, S](p, schema)
  transparent inline def append[P <: RecordProperty0](p: P): RecordSchema =
    schema match
      case EmptySchema => SchemaCons[P, S](p, schema)
      case SchemaCons(pp, s) => 
        val sa = s.append(p)
        SchemaCons[pp.type, sa.type](pp, sa)
end extension

def sprepend[S <: RecordSchema, P <: RecordProperty0](schema: S, p: P): SchemaCons[P, S] =
    SchemaCons[P, S](p, schema)

trait RecordSchemaBuilderBase:
  type RecordType

trait SchemaBuilder extends RecordSchemaBuilderBase:
  transparent inline def emptySchema = RecordSchema.empty
  /** This macro doesn't work at the moment. */
  transparent inline def fields(inline properties: RecordProperty0*): RecordSchema =
    ${ fieldsImpl[EmptySchema]('{properties}, '{RecordSchema.empty}) }

  transparent inline def fields1[P1<:RecordProperty0](inline p1: P1) = 
    sprepend(RecordSchema.empty, p1)
  transparent inline def fields2[P1<:RecordProperty0, P2<:RecordProperty0](inline p1: P1, inline p2: P2) = 
    sprepend(RecordSchema.empty, p2).prepend(p1)
  transparent inline def fields3[P1<:RecordProperty0, P2<:RecordProperty0, P3<:RecordProperty0](inline p1: P1, inline p2: P2, inline p3: P3) = 
    sprepend(RecordSchema.empty, p3).prepend(p2).prepend(p1)

  transparent inline def tupleToSchema[T <: Tuple](inline t: T)(using inline ev: Tuple.IsMappedBy[RecordProperty][T]): RecordSchema = 
    ${tupleToSchemaImpl('t, 'ev)}

  transparent inline def showExpr(inline a: Any): String = 
    ${showExprImpl('a)}
    
  transparent inline def traitExpr(inline a: Any): Any = 
    ${traitExprImpl('a)}
    

def fieldsImpl[S <: RecordSchema](
  propertyList: Expr[Seq[RecordProperty0]], 
  schemaExpr: Expr[S]
)(using Type[S])(using Quotes): Expr[RecordSchema] =
  propertyList match
    case Varargs(Seq())  => 
      schemaExpr
    case Varargs(Seq('{ $a: ta }, as*))  => //[S, ta&RecordProperty0]
      // val expr = '{ sprepend(${fieldsImpl(Varargs(as), schemaExpr)}, ${a})}
      val expr = fieldsImpl(Varargs(as), '{ sprepend(${schemaExpr}, ${a})} )
      println(expr.show)
      expr

class RecordSchemaBuilder[R] extends PropertiesBuilder with ForeignKeyBuilder with SchemaBuilder:
  type RecordType = R

def tupleToSchemaImpl[T<:Tuple](t: Expr[T], ev: Expr[Tuple.IsMappedBy[RecordProperty][T]])(using Quotes): Expr[RecordSchema] = 
  t match 
      case '{EmptyTuple} => '{EmptySchema}
      case '{($a: RecordProperty0, $b: RecordProperty0)} => 
        '{EmptySchema.prepend($b).prepend($a)}//tupleToSchemaImpl(ts)
          // case _ => '{EmptySchema}
      case _ => 
        println(t.show)
        '{EmptySchema}
      // case '{($h:ht) *: ($ts: Tuple)} => 
      //   // if h.
      //   // h.show
      //   tupleToSchemaImpl(ts)
      // case _ => '{EmptySchema}
  //     case '{($p: RecordProperty0) *: (${ts}: Tuple)} => '{sprepend(${tupleToSchemaImpl(ts)}, $p)}
def showExprImpl(a: Expr[Any])(using Quotes): Expr[String] = 
  Expr(a.show)

def traitExprImpl(a: Expr[Any])(using Quotes): Expr[Any] = 
  '{
    trait Hello{
      val name: String = ""
    }
    new Hello {}
  }
