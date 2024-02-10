/**
  * Package `simplemeta2` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simplemeta3

import scala.language.higherKinds
import ru.primetalk.typed.ontology.metameta.OntologyType.Record
import scala.quoted.*
import scala.quoted.Expr.ofList

import scala.reflect.ClassTag
import scala.compiletime.ops.int._
import scala.compiletime.constValue
import scala.compiletime.constValueTuple
import ru.primetalk.typed.ontology.utils.objectName

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

  type PropertySet = Tuple.Union[Properties] & RecordProperty0
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
  inline def values(inline v: Values): Values = v

  type IndexOfProp[P] = 
    RecordSchema.IndexOfTypeInTuple[Properties, P]

  transparent inline def indexOfProp[This >: this.type <: RecordSchema, P2 <: PropertySet](p2: P2): IndexOfProp[p2.type]

  def indexOfPropR[This >: this.type <: RecordSchema](p2: RecordProperty0): Int =
    ???
  type IndicesOfProps[S2<: RecordSchema] <: Tuple =
    S2 match
      case EmptySchema => EmptyTuple
      case SchemaCons[p, s] => 
        IndexOfProp[p] *: 
          IndicesOfProps[s]

  transparent inline def indicesOfProps[This >: this.type <: RecordSchema, S2 <: RecordSchema](s2: S2): IndicesOfProps[s2.type] = 
    constValueTuple[IndicesOfProps[s2.type]]

  transparent inline def getByIndex[I <: Int](inline i: I)(v: Values): ValueAt[I] =
    RecordSchema.valueAt(this, i)(v)

  def getByIndexRuntime(i: Int)(v: Values): Any =
    scala.runtime.Tuples.apply(v.asInstanceOf[NonEmptyTuple], i)

  type ValueAt[I] = 
    I match
      case Int =>
        RecordSchema.ValueAt[this.type, I]
      case _ => 
        Nothing

  // transparent inline def projection[S2 <: RecordSchema](inline schema2: S2): Values => schema2.Values = 
  //   schema2 match
  //     case EmptySchema      => v => EmptyTuple.asInstanceOf[schema2.Values]
  //     case SchemaCons(p, s) => v => EmptyTuple.asInstanceOf[schema2.Values]//??? // this.propertyGetter
    // inline val indices = indicesOfProps(schema2)
    // Converter.converter(this, schema2)(indices)
  // // THIS HANGS!
  // transparent inline def projection[S2 <: RecordSchema](inline schema2: S2): Values => schema2.Values = 
  //   inline val indices = indicesOfProps(schema2)
  //   Converter.converter(this, schema2)(indices)
    
  transparent inline def concat[This >: this.type <: RecordSchema, S2 <: RecordSchema](schema2: S2): RecordSchema.Concat[This, schema2.type] =
    inline this match
      case _: EmptySchema => 
        schema2
      case sc: SchemaCons[p, s] => 
        (sc.schema.concat(schema2).prepend(sc.p))

  transparent inline def concatValues[This >: this.type <: RecordSchema, S2 <: RecordSchema](schema2: S2): (Values, schema2.Values) => RecordSchema.Concat[This, S2] = 
    ???

  type PropertyGetter[P] = 
    Values => RecordProperty0.PropertyValueType[P] // Tuple.Elem[Values, IndexOfProp[P]] 

  type FromOtherValuesGetter[V] = 
    V => Tuple.Map[Properties, RecordProperty0.PropertyValueType]
    
  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P](p: P): PropertyGetter[p.type]

  // type ProjectorFrom[S1<: RecordSchema] = S1#Values => RecordProperty0.PropertyValueType[P]// (inline is: InverseIndicesOfProps[s1.type]/*IndicesOfProps[This]*/)
  // type InverseIndicesOfProps[S1 <: RecordSchema] = S1#IndicesOfProps[this.type]
  // transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: NonEmptySchema](inline s1: S1): s1.Values => Values
  transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: RecordSchema](s1: S1): FromOtherValuesGetter[s1.Values]

object Converter:
  transparent inline def converter[S1<: RecordSchema, S2<:RecordSchema](s1: S1, s2: S2)(indices: s1.IndicesOfProps[S2]): s1.Values => s2.Values =
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

  transparent inline def indexOfProp[This >: this.type <: RecordSchema, P2 <: PropertySet](p2: P2): IndexOfProp[p2.type] =
    RecordSchema.indexOfProp(this, p2)
  // transparent inline def projection[S2 <: RecordSchema](inline schema2: S2): Values => schema2.Values =
  //   inline schema2 match
  //     case EmptySchema => _ => EmptyTuple.asInstanceOf[schema2.Values]
  //     case _ => _ => throw new IllegalArgumentException(s"Cannot project $this on $schema2")

  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P](p: P): PropertyGetter[p.type] = 
      sys.error(s"There is no property getter for $p in empty schema")

  // transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: NonEmptySchema](inline s1: S1)(inline pg: s1.PropertyGetter[p.type]): s1.Values => Values =
  // transparent inline def projectorFrom[S1<: RecordSchema](inline s1: S1)(using ev: PropertySet <:< s1.PropertySet): s1.Values => Values =
  transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: RecordSchema](s1: S1): FromOtherValuesGetter[s1.Values] =
    _ => EmptyTuple
    
sealed trait NonEmptySchema extends RecordSchema:
  type Properties <: NonEmptyTuple
  type ValuesElem[I <: Int] = Tuple.Elem[Values, I]
  transparent inline def valueAt[N<: Int](n: N): Values => Tuple.Elem[Values, N] = 
    v => v.asInstanceOf[NonEmptyTuple].apply(n).asInstanceOf[Tuple.Elem[Values, N]]

final case class SchemaCons[P <: RecordProperty0, S <: RecordSchema](p: P, schema: S) extends NonEmptySchema:
  import RecordSchema._
  def indexOfThisProp(p: P): 0 = 0
  type R = p.R | schema.R

  type PValue = RecordProperty0.PropertyValueType[p.type]

  type ParentSchemaOrNothing = schema.type
  type Properties = p.type *: schema.Properties
  val properties: Properties = p *: schema.properties
  def parentSchemaOrNothing: ParentSchemaOrNothing = schema
  def get[P2 <: RecordProperty0](p2: P2)(v: Values): Option[p2.P] = 
    v match
      case head *: (tail: schema.Values) =>
        if p2 == p then
          Some(head.asInstanceOf[p2.P])
        else
          schema.get(p2)(tail)
  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] =
    v match
      case head *: (tail: schema.Values) =>
        schema.convertToMap(tail, m.updated(p.name, head))
  def isEmpty: Boolean = false

  def unapply[This >: this.type <: SchemaCons[P, S]]: Unapply[This] =
    Some((p, schema))
  transparent inline def indexOfProp[This >: this.type <: RecordSchema, P2](p2: P2): IndexOfProp[p2.type] =
    RecordSchema.indexOfProp(this, p2)

  // //   // 0.asInstanceOf[IndexOfProp[This, p2.type, 0]]//constValue[IndexOfProp[This, p2.type, 0]]
  // transparent inline def propertyGetter0[
  //   This >: this.type <: RecordSchema, 
  //   I <: Int](inline i: I): 
  //     this.Values => Tuple.Elem[this.Values, I] =
  //   _.apply(i).asInstanceOf[Tuple.Elem[this.Values, I]]
  //   // ${ propertyGetterImpl[this.Values, I]('i)}
  // DONE: Test property getter. make sure it works
  // TODO: Try to combine a few property getters
  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P](p: P): PropertyGetter[p.type] =
    val i = indexOfProp(p)
    _.apply(i).asInstanceOf[RecordProperty0.PropertyValueType[p.type]]////Tuple.Elem[Values, IndexOfProp[p.type]]]
    

  transparent inline def projection[S2 <: RecordSchema](schema2: S2)(using ev: schema2.PropertySet <:< PropertySet): Values => schema2.Values =
    // schema2.projectorFrom(this)
    // val getters = schema2.properties.map[PropertyGetter]([t] => (p: t) => propertyGetter(p))
    ???
    // inline schema2 match
    //   case EmptySchema => _ => EmptyTuple.asInstanceOf[schema2.Values]
    //   case SchemaCons(p, s) =>
    //     ??? 
        // this.propertyGetter(p)
        //_ => throw new IllegalArgumentException(s"Cannot project $this on $schema2")
  // transparent inline def projectorFrom[S1 <: RecordSchema](inline s1: S1)(using ev1: PropertySet <:< s1.PropertySet): s1.Values => Values =
  //   // val i = s1.indexOfProp(ev1(p))
  //   v => s1.propertyGetter(p)(v) *: schema.projectorFrom(s1)(v)
  // transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: NonEmptySchema](inline s1: S1)(inline pg: s1.PropertyGetter[p.type]): s1.Values => Values =
  //   schema match
  //     case EmptySchema =>
  //       (v: s1.Values) => 
  //         pg(v) *: EmptyTuple
  //     case NonEmptySchema => 
  //       val p1 = schema.projectorFrom(s1)
  //       // val pg = s1.propertyGetter(p)
  //       (v: s1.Values) => 
  //         pg(v) *: p1(v)
  //     // val valueAt: [t] => (n: t) => s1.ValuesElem[t] = 
  //     //   [N] => (n: N) => s1.valueAt[N](n)(v)//v.apply(n)
  //     // is.map[s1.ValuesElem[_]](valueAt).asInstanceOf[Values]
      // is.map[s1.ValuesElem[_]]([n] => (i: n) => valueAt[n](i)).asInstanceOf[Values]
//   transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: NonEmptySchema](inline s1: S1): s1.Values => Values =
//     ${projectorFromImpl[This, S1, s1.Values, this.Values]('this, 's1)}

// def propertyGetterImpl[S1 <: RecordSchema, P <: RecordProperty0, V1<: Tuple](s1: Expr[S1], p: Expr[P])(using s1t: Type[S1], v1t: Type[V1], pt: Type[P]): Expr[V1] => Expr[Any] = 
//   ???
// def projectorFromImpl0[This <: RecordSchema, S1 <: RecordSchema, V1<: Tuple, V2 <: Tuple](self: Expr[This], s1: Expr[S1]): Expr[V1] => Expr[V2] = 
//   ???
// def projectorFromImpl[This <: RecordSchema, S1 <: RecordSchema, V1<: Tuple, V2 <: Tuple](self: Expr[This], s1: Expr[S1])(using Quotes): Expr[V1 => V2] = 
//   import quotes.reflect._
//   val tpeS1Values = Select(s1.asTerm, Symbol.classSymbol(RecordSchema.getClass.getName).typeMember("Values")
//   // println(tpeS1Values.show)
//   // ???
//   projectorFromImpl0(self, s1)(???)
  transparent inline def projectorFrom[This >: this.type <: RecordSchema, S1 <: RecordSchema](s1: S1): FromOtherValuesGetter[s1.Values] =
    // val p2 = p.asInstanceOf[s1.PropertySet]
    val f2 = s1.propertyGetter(p)
    val f1: s1.Values => PValue = 
      s1.propertyGetter(p)
      // f2//.asInstanceOf[s1.Values => RecordProperty0.PropertyValueType[p.type]]
    val p1: schema.FromOtherValuesGetter[s1.Values] = schema.projectorFrom(s1)
    (v: s1.Values) => {
      val f1v: PValue = f1(v)
      val p1v: schema.Values = p1(v)
      f1v *: 
        p1v
    }

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

  type ValueAt[X <: RecordSchema, N <: Int] <: Any = X match
    case EmptySchema => Nothing
    case SchemaCons[p, s] => 
      N match
        case 0    => RecordProperty0.PropertyValueType[p]
        case S[n] => ValueAt[s, n]

  transparent inline def valueAt[X <: RecordSchema, I <: Int](schema: X, inline i: I)(v: schema.Values): ValueAt[X, I] = 
    scala.runtime.Tuples.apply(v.asInstanceOf[NonEmptyTuple], i).asInstanceOf[ValueAt[X, I]]

    // schema match
    //   case EmptySchema => ???
    //   case SchemaCons(p1, s1): SchemaCons[p,s] =>
    //     i match
    //       case 0 => v(0).asInstanceOf[p#P]
    //       case S(i) => valueAt(s1, i - 1)
  type PropByProp[X <: RecordSchema, P <: RecordProperty0] <: RecordProperty0 = X match
    case EmptySchema => Nothing
    case SchemaCons[P, s] => P
    case SchemaCons[_, s] => PropByProp[s, P]

  transparent inline def indexOfProp[S1 <: RecordSchema, P](schema: S1, inline property: P): schema.IndexOfProp[P] = 
    constValue[schema.IndexOfProp[P]]

  transparent inline def indicesOfProps[S1 <: RecordSchema, S2 <: RecordSchema](s1: S1, inline s2: S2): s1.IndicesOfProps[S2] = 
    constValueTuple[s1.IndicesOfProps[S2]]

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
 
  inline def empty: EmptySchema = EmptySchema

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

abstract class TableBuilder extends PropertiesBuilder with ForeignKeyBuilder with SchemaBuilder:
  type RecordType = this.type

  protected abstract class column[T: ClassTag] extends SimplePropertyId[RecordType, T]("", summon):
    override val name = objectName(this)

  type TableSchema <: RecordSchema
  val tableSchema: TableSchema
  type Row = tableSchema.Values

  trait Relation1 {
    type Schema = tableSchema.type
    val schema = tableSchema
  }
  transparent inline def relation0(inline values1: List[Row]) = 
    new Relation1 {
      val values = values1
    }

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
