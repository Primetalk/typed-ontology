/**
  * Package `simplemeta2` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simplemeta

import scala.language.higherKinds
import ru.primetalk.typed.ontology.Record
import scala.quoted.*
import scala.quoted.Expr.ofList

import scala.reflect.ClassTag
import scala.compiletime.ops.int._
import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.constValueTuple
import java.sql.ResultSet
import scala.annotation.targetName

/** 
  * Schema of properties for record R.
  * We can use tuples to represent instances of this schema.
  */
sealed trait RecordSchema: 
  self =>
  import RecordSchema._
  
  type Properties  <: Tuple
  val properties: Properties

  override def toString: String =
    properties.toIArray.mkString(", ")

  type PropertySet = Tuple.Union[Properties] & RecordProperty0

  type Values      = Tuple.Map[Properties, RecordProperty0.PropertyValueType]

  // Higher order operation on each value.
  // Could be used to construct more complex data structures. For instance, tuple of options, or tuple of Either[Error, _]
  type HValues[H[_]] = Tuple.Map[Values, H]

  def get[P2 <: RecordProperty0](p2: P2)(v: Values): Option[p2.P]

  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any]

  type IndexOfProp[P <: RecordProperty0] = RecordSchema.IndexOfTypeInTuple[Properties, P]
  transparent inline def indexOfProp[This >: this.type <: RecordSchema, P2 <: RecordProperty0](inline p2: P2): IndexOfProp[p2.type] =
    RecordSchema.indexOfProp(this, p2)

  type IndicesOfProps[S2 <: RecordSchema] <: Tuple =
    S2 match
      case EmptySchema      => EmptyTuple
      case SchemaCons[p, s] => IndexOfProp[p] *: IndicesOfProps[s]
  transparent inline def indicesOfProps[This >: this.type <: RecordSchema, S2 <: RecordSchema](inline s2: S2): IndicesOfProps[s2.type] = 
    constValueTuple[IndicesOfProps[s2.type]]

  type ValueAt[I] = 
    I match
      case Int =>
        RecordSchema.ValueAt[this.type, I]
      case _ => 
        Nothing
  transparent inline def getByIndex[I <: Int](inline i: I)(v: Values): ValueAt[I] =
    RecordSchema.valueAt(this, i)(v)

  def getByIndexRuntime(i: Int)(v: Values): Any =
    scala.runtime.Tuples.apply(v.asInstanceOf[NonEmptyTuple], i)

  transparent inline def concat[S2 <: RecordSchema](inline schema2: S2): RecordSchema.Concat[this.type, schema2.type] =
    inline this match
      case _: EmptySchema => 
        schema2
      case sc: SchemaCons[p, s] => 
        sc.p #: sc.schema.concat(schema2)

  transparent inline def concatValues[This >: this.type <: RecordSchema, S2 <: RecordSchema](inline schema2: S2): (Values, schema2.Values) => Tuple.Concat[Values, schema2.Values] = 
    (v1, v2) => v1 ++ v2

  type PropertyGetter[P <: RecordProperty0] = 
    Values => RecordProperty0.PropertyValueType[P]
  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P <: RecordProperty0](inline p: P): PropertyGetter[p.type]

  type ValuesGetter[-V] = 
    V => Values
    //Tuple.Map[Properties, RecordProperty0.PropertyValueType]
    
  // type ProjectorFrom[S1<: RecordSchema] = S1#Values => RecordProperty0.PropertyValueType[P]// (inline is: InverseIndicesOfProps[s1.type]/*IndicesOfProps[This]*/)
  // type InverseIndicesOfProps[S1 <: RecordSchema] = S1#IndicesOfProps[this.type]
  // transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: NonEmptySchema](inline s1: S1): s1.Values => Values
  transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: RecordSchema](inline s1: S1): ValuesGetter[s1.Values]

  transparent inline def projection[S2 <: RecordSchema](inline schema2: S2): Values => schema2.Values = 
    schema2.projectorFrom(this)

  @targetName("SchemaCons")
  inline def #: [P <: RecordProperty0, This >: this.type <: RecordSchema] (p: P): P #: This =
    SchemaCons[P, This](p, this)
    
object Converter: // TODO: construct expression to convert values from one schema to another
  transparent inline def converter[S1<: RecordSchema, S2<:RecordSchema](inline s1: S1, inline s2: S2)(indices: s1.IndicesOfProps[S2]): s1.Values => s2.Values =
    (v: s1.Values) => 
      val valueAt: [I] => (i:I) => s1.ValueAt[I] =
        [I] => (i: I) => s1.getByIndexRuntime(i.asInstanceOf[Int])(v).asInstanceOf[s1.ValueAt[I]]

      scala.runtime.Tuples.map[s1.ValueAt](indices, valueAt).asInstanceOf[s2.Values]

type EmptySchema = EmptySchema.type

case object EmptySchema extends RecordSchema:

  import RecordSchema._
  type R = Nothing
  
  type ParentSchemaOrNothing = Nothing
  type Properties = EmptyTuple
  val properties: Properties = EmptyTuple

  def get[P2 <: RecordProperty0](p2: P2)(v: Values): Option[p2.P] = 
    None
  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] = 
    m

  def unapply(e: EmptySchema): true = true

  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P<: RecordProperty0](inline p: P): PropertyGetter[p.type] = 
      sys.error(s"There is no property getter for $p in empty schema")

  // transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: NonEmptySchema](inline s1: S1)(inline pg: s1.PropertyGetter[p.type]): s1.Values => Values =
  // transparent inline def projectorFrom[S1<: RecordSchema](inline s1: S1)(using ev: PropertySet <:< s1.PropertySet): s1.Values => Values =
  transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: RecordSchema](inline s1: S1): ValuesGetter[s1.Values] =
    _ => EmptyTuple
    
sealed trait NonEmptySchema extends RecordSchema:
  type Properties <: NonEmptyTuple
  type ValuesElem[I <: Int] = Tuple.Elem[Values, I]
  transparent inline def valueAt[N<: Int](n: N): Values => Tuple.Elem[Values, N] = 
    v => v.asInstanceOf[NonEmptyTuple].apply(n).asInstanceOf[Tuple.Elem[Values, N]]

final case class SchemaCons[P <: RecordProperty0, S <: RecordSchema](p: P, schema: S) extends NonEmptySchema:
  import RecordSchema._
  def indexOfThisProp(p: P): 0 = 0

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

  def unapply[This >: this.type <: SchemaCons[P, S]]: Unapply[This] =
    Some((p, schema))

  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P <: RecordProperty0](inline p: P): PropertyGetter[p.type] =
    val i = indexOfProp(p)
    _.apply(i).asInstanceOf[RecordProperty0.PropertyValueType[p.type]]////Tuple.Elem[Values, IndexOfProp[p.type]]]
    
  transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: RecordSchema](inline s1: S1): ValuesGetter[s1.Values] =
    // val p2 = p.asInstanceOf[s1.PropertySet]
    val f2 = s1.propertyGetter(p)
    val f1: s1.Values => RecordProperty0.PropertyValueType[p.type] = 
      s1.propertyGetter(p)

    val p1: schema.ValuesGetter[s1.Values] = schema.projectorFrom(s1)
    (v: s1.Values) => f1(v) *: p1(v)

infix type #:[P <: RecordProperty0, S <: RecordSchema] = SchemaCons[P, S]

object RecordSchema:
  type IndexOfTypeInTupleAux[T<:Tuple, A, N <: Int] <: Int = T match
    case EmptyTuple => Nothing
    case A *: t => N
    case _ *: t => IndexOfTypeInTupleAux[t, A, S[N]]

  type IndexOfTypeInTuple[T<:Tuple, A] = IndexOfTypeInTupleAux[T, A, 0]

  type Unapply[X <: RecordSchema] = X match
    case EmptySchema      => None.type
    case SchemaCons[p, s] => Some[(p,s)]

  type PropAt[X <: RecordSchema, N <: Int] <: RecordProperty0 = X match
    case EmptySchema => Nothing
    case SchemaCons[p, s] => 
      N match
        case 0 => p
        case S[n] => PropAt[s, n]

  type ValueAt[X <: RecordSchema, N <: Int] <: Any = X match
    case EmptySchema => Nothing
    case SchemaCons[p, s] => 
      N match
        case 0    => RecordProperty0.PropertyValueType[p]
        case S[n] => ValueAt[s, n]

  transparent inline def valueAt[X <: RecordSchema, I <: Int](inline schema: X, inline i: I)(v: schema.Values): ValueAt[X, I] = 
    scala.runtime.Tuples.apply(v.asInstanceOf[NonEmptyTuple], i).asInstanceOf[ValueAt[X, I]]

  transparent inline def indexOfProp[S1 <: RecordSchema, P <: RecordProperty0](inline schema: S1, inline property: P): schema.IndexOfProp[P] = 
    constValue[schema.IndexOfProp[P]]

  transparent inline def indicesOfProps[S1 <: RecordSchema, S2 <: RecordSchema](inline s1: S1, inline s2: S2): s1.IndicesOfProps[S2] = 
    constValueTuple[s1.IndicesOfProps[S2]]

  /** Type of the concatenation of two schemas. */
  type Concat[X <: RecordSchema, Y <: RecordSchema] <: RecordSchema = 
    X match 
      case EmptySchema => Y
      case SchemaCons[x1, xs1] => SchemaCons[x1, Concat[xs1, Y]]
 
  def empty: EmptySchema = EmptySchema

  type TupleToSchema[T <: Tuple] <: RecordSchema =
    T match
      case EmptyTuple => EmptySchema
      case p *: t => p #: TupleToSchema[t]

  transparent inline def constSchema[S <: RecordSchema]: S =
    inline erasedValue[S] match
      case _: EmptySchema      => RecordSchema.empty.asInstanceOf[S]
      case _: SchemaCons[p, s] =>
        (constValue[p] #: constSchema[s]).asInstanceOf[S]


extension [S <: RecordSchema](schema: S)
  transparent inline infix def prepend_:(inline p: RecordProperty0) =
    SchemaCons[p.type, S](p, schema)
  infix transparent inline def append[P <: RecordProperty0](inline p: P): RecordSchema =
    schema match
      case EmptySchema => SchemaCons[P, S](p, schema)
      case SchemaCons(pp, s) => 
        val sa = s.append(p)
        SchemaCons(pp, sa)
end extension

transparent inline infix def sprepend[S <: RecordSchema, P <: RecordProperty0](inline schema: S, inline p: P): SchemaCons[P, S] =
    SchemaCons[P, S](p, schema)

trait RecordSchemaBuilderBase:
  type RecordType

trait SchemaBuilder extends RecordSchemaBuilderBase:
  transparent inline def emptySchema = RecordSchema.empty
  /** This macro doesn't work at the moment. */
  transparent inline def fields(inline properties: RecordProperty0*): RecordSchema =
    ${ fieldsImpl[EmptySchema]('{properties}, '{RecordSchema.empty}) }

  transparent inline def fields1[P1<:RecordProperty0](inline p1: P1) = 
    p1 #: RecordSchema.empty
  transparent inline def fields2[P1<:RecordProperty0, P2<:RecordProperty0](inline p1: P1, inline p2: P2) = 
    p1 #: p2 #: RecordSchema.empty
  transparent inline def fields3[P1<:RecordProperty0, P2<:RecordProperty0, P3<:RecordProperty0](inline p1: P1, inline p2: P2, inline p3: P3) = 
    p1 #: p2 #: p3 #: RecordSchema.empty

  transparent inline def tupleToSchema[T <: Tuple](inline t: T): RecordSchema.TupleToSchema[T] = 
    ${tupleToSchemaImpl('t)}

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

abstract class TableBuilder extends PropertiesBuilder with ForeignKeyBuilder with SchemaBuilder:
  type RecordType = this.type

  protected abstract class column[T: ClassTag] extends SimplePropertyId[RecordType, T]("", summon):
    override val name = this.getClass.getSimpleName.replace("$", "")

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
def showExprImpl(a: Expr[Any])(using Quotes): Expr[String] = 
  Expr(a.show)

def traitExprImpl(a: Expr[Any])(using Quotes): Expr[Any] = 
  '{
    trait Hello{
      val name: String = ""
    }
    new Hello {}
  }

  // transparent inline def propertyGetter[P<:PropertySet](inline p: P): Values => Tuple.Elem[Values, IndexOfProp[p.type]] = //  RecordProperty0.PropertyValueType[p.type]
  //   ${ propertyGetterImpl('p)}
  //   inline val i: IndexOfProp[p.type] = indexOfProp(p)
  //   (v: Values) => v.apply[Values](i)//scala.runtime.Tuples.apply(v, i)

def to[T, R: Type](f: Expr[T] => Expr[R])(using t: Type[T])(using Quotes): Expr[T => R] =
  '{ (x: t.Underlying) => ${ f('x) } }

def propertyGetterImpl[V <: NonEmptyTuple, I<:Int](i: Expr[Int])(using vt: Type[V], rt: Type[I])(using Quotes): Expr[V => Tuple.Elem[V, I]] = 
  '{
    // import quotes.reflect._
    (v: V) => v.apply($i).asInstanceOf[Tuple.Elem[V, I]]
  }
