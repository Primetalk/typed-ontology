/**
  * Package `simple.meta` contains definitions that are used to express ontology.
  * For instance, here we define PropertyId class that represent a property description.
  * We might want to put any additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simple.meta

import scala.language.higherKinds
import ru.primetalk.typed.ontology.metameta.Record
import scala.quoted.*
import scala.quoted.Expr.ofList

import scala.reflect.ClassTag
import scala.compiletime.ops.int._
import scala.compiletime.summonInline
import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.constValueTuple
import java.sql.ResultSet
import scala.annotation.targetName
import scala.runtime.Tuples

/** 
  * Schema of properties for a record R.
  * We can use tuples and maps to represent instances of this schema.
  */
sealed trait RecordSchema: 
  self =>
  import RecordSchema._
  
  type Properties  <: Tuple
  val properties: Properties

  override def toString: String =
    properties.toIArray.mkString(", ")

  type PropertySet = Tuple.Union[Properties] & RecordProperty0

  /** A type function that applies a given type function to each property.*/
  type PropertiesMap[F[_]] = Tuple.Map[Properties, F]
  
  type IndexOfProp[P <: RecordProperty0] = RecordSchema.IndexOfTypeInTuple[Properties, P]
  transparent inline def indexOfProp[P2 <: RecordProperty0](inline p2: P2): IndexOfProp[p2.type] =
    RecordSchema.indexOfProp(this, p2)

  /** Returns the tuple made of indices of properties of another schema.
   */
  type IndicesOfProps[S2 <: RecordSchema] <: Tuple =
    S2 match
      case EmptySchema      => EmptyTuple
      case SchemaCons[p, s] => IndexOfProp[p] *: IndicesOfProps[s]
  transparent inline def indicesOfProps[S2 <: RecordSchema](inline s2: S2): IndicesOfProps[s2.type] = 
    constValueTuple[IndicesOfProps[s2.type]]

  /** Concatenates properties of another schema. */
  // transparent inline def concat[S2 <: RecordSchema, This >: this.type <: RecordSchema](inline schema2: S2): RecordSchema.Concat[This, schema2.type] =
  inline def concat[S2 <: RecordSchema](inline schema2: S2): RecordSchema.Concat[this.type, schema2.type] =
    inline this match
      case _: EmptySchema => 
        schema2
      case sc: SchemaCons[p, s] => 
        sc.p #: sc.schema.concat(schema2)

  /** Type of the concatenation of two schemas. */
  type PrependOtherSchema[S1 <: RecordSchema] <: RecordSchema = 
    S1 match 
      case EmptySchema      => this.type
      case SchemaCons[p, s] => SchemaCons[p, PrependOtherSchema[s]]

  // DOESN'T WORK!
  transparent inline def prependOtherSchema[S1 <: RecordSchema](inline s1: S1): PrependOtherSchema[S1] =
    inline s1 match
      case _: EmptySchema       => 
        this
      case sc: SchemaCons[p, s] => 
        val ps: PrependOtherSchema[s] = prependOtherSchema[s](sc.schema)
        SchemaCons[p, PrependOtherSchema[s]](sc.p, ps)
  /** Type of the concatenation of two schemas. */
  type AppendOtherSchema[S2 <: RecordSchema] <: RecordSchema

  transparent inline def appendOtherSchema[S2 <: RecordSchema](inline s2: S2): AppendOtherSchema[S2]
 
  @targetName("SchemaCons")
  inline def #: [P <: RecordProperty0, This >: this.type <: RecordSchema] (p: P): P #: This =
    SchemaCons[P, This](p, this)

  transparent inline def replace[P1 <: RecordProperty0, P2 <: RecordProperty0](inline p1: P1, inline p2: P2): RecordSchema

  /** Replaces properties of the same type. A bit more restricted version of replace. */
  transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T]](inline p1: P1, inline p2: P2): RecordSchema =
    replace(p1, p2)

  type Remove[P1<:RecordProperty0] <: RecordSchema

  transparent inline def remove[P1 <: RecordProperty0](inline p1: P1): Remove[p1.type]



  // The subsequent elements might eventually be moved from schema.
  // Currently it doesn't seem to work outside of this trait...

  /** Simple tuple representing an instance of this schema. */
  type Values      = PropertiesMap[RecordProperty0.PropertyValueType]

  // Higher order operation on each value.
  // Could be used to construct more complex data structures. For instance, tuple of options, or tuple of Either[Error, _]
  type ValuesMap[H[_]] = Tuple.Map[Values, H]

  def get[P2](p2: P2)(v: Values): Option[RecordProperty0.PropertyValueType[p2.type]]

  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any]

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

  transparent inline def concatValues[S2 <: RecordSchema](inline schema2: S2)(inline schema3: RecordSchema.Concat[this.type, schema2.type]): (Values, schema2.Values) => schema3.Values = 
      (v1, v2) => (v1 ++ v2).asInstanceOf[schema3.Values]

  transparent inline def prependValues[S1 <: RecordSchema](inline schema1: S1)(inline schema3: PrependOtherSchema[S1]): (schema1.Values, Values) => schema3.Values = 
    (v1, v2) => (v1 ++ v2).asInstanceOf[schema3.Values]

  transparent inline def appendValues[S2 <: RecordSchema](inline schema2: S2)(inline schema3: AppendOtherSchema[S2]): (Values, schema2.Values) => schema3.Values =
    (v1, v2) => (v1 ++ v2).asInstanceOf[schema3.Values]

  type PropertyGetter[P <: RecordProperty0] = 
    Values => RecordProperty0.PropertyValueType[P]
  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P <: RecordProperty0](inline p: P): PropertyGetter[p.type]

  type PropertyUpdater[P <: RecordProperty0] = 
    Values => RecordProperty0.PropertyValueType[P] => Values
  transparent inline def propertyUpdater[
    This >: this.type <: RecordSchema, 
    P <: RecordProperty0](inline p: P): PropertyUpdater[p.type]

  transparent inline def projectorFrom[S1 <: RecordSchema](inline s1: S1): s1.Values => Values

  transparent inline def projection[S2 <: RecordSchema](inline schema2: S2): Values => schema2.Values = 
    schema2.projectorFrom(this)

  type OptionValues = ValuesMap[Option]

  transparent inline def transformOption: OptionValues => Option[Values]
     
  type EitherValues[E] = ValuesMap[[V] =>> Either[E, V]]

  transparent inline def transformEither[E]: EitherValues[E] => Either[List[E], Values]
  
  transparent inline def fkPredicate[FK <: ForeignKeyId0](inline fk: FK): Values => Boolean = 
    val l = propertyGetter(fk.left)
    val r = propertyGetter(fk.right)
    row => l(row) == r(row)

  extension (values: Values)
    transparent inline def apply[P <: RecordProperty0](inline p: P) = 
      self.propertyGetter(p)(values)
    transparent inline def updated[P <: RecordProperty0](inline p: P)(inline v: RecordProperty0.PropertyValueType[p.type]) = 
      self.propertyUpdater(p)(values)(v)

type EmptySchema = EmptySchema.type

case object EmptySchema extends RecordSchema:

  import RecordSchema._
  type R = Nothing
  
  type ParentSchemaOrNothing = Nothing
  type Properties = EmptyTuple
  val properties: Properties = EmptyTuple

  def unapply(e: EmptySchema): true = true

  type AppendOtherSchema[S2 <: RecordSchema] = S2
  transparent inline def appendOtherSchema[S2 <: RecordSchema](inline s2: S2): AppendOtherSchema[S2] = 
    s2

  transparent inline def replace[P1 <: RecordProperty0, P2 <: RecordProperty0](inline p1: P1, inline p2: P2): RecordSchema =
    EmptySchema

  type Remove[P1<:RecordProperty0] = EmptySchema

  transparent inline def remove[P1 <: RecordProperty0](inline p1: P1): Remove[p1.type] = EmptySchema

  def get[P2](p2: P2)(v: Values): Option[RecordProperty0.PropertyValueType[p2.type]] = 
    None
  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] = 
    m

  transparent inline def propertyGetter[
    This >: this.type <: RecordSchema, 
    P<: RecordProperty0](inline p: P): PropertyGetter[p.type] = 
      sys.error(s"There is no property getter for $p in empty schema")
  transparent inline def propertyUpdater[
    This >: this.type <: RecordSchema, 
    P <: RecordProperty0](inline p: P): PropertyUpdater[p.type] = 
      sys.error(s"There is no property updater for $p in empty schema")

  transparent inline def projectorFrom[S1 <: RecordSchema](inline s1: S1): s1.Values => Values =
    _ => EmptyTuple
  transparent inline def transformOption: OptionValues => Option[Values] =
    _ => Some(EmptyTuple)

  transparent inline def transformEither[E]: EitherValues[E] => Either[List[E], Values] =
    _ => Right(EmptyTuple)

sealed trait NonEmptySchema extends RecordSchema:
  type Properties <: NonEmptyTuple
  type ValuesElem[I <: Int] = Tuple.Elem[Values, I]
  transparent inline def valueAt[N<: Int](n: N): Values => Tuple.Elem[Values, N] = 
    v => v.asInstanceOf[NonEmptyTuple].apply(n).asInstanceOf[Tuple.Elem[Values, N]]

final case class SchemaCons[P <: RecordProperty0, S <: RecordSchema](p: P, schema: S) extends NonEmptySchema:
  import RecordSchema._

  type ParentSchemaOrNothing = schema.type
  type Properties = p.type *: schema.Properties
  val properties: Properties = p *: schema.properties
  def parentSchemaOrNothing: ParentSchemaOrNothing = schema
  def get[P2](p2: P2)(v: Values): Option[RecordProperty0.PropertyValueType[p2.type]] = 
    if p2 == p then
      Some(v.head.asInstanceOf[RecordProperty0.PropertyValueType[p2.type]])
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
  transparent inline def propertyUpdater[
    This >: this.type <: RecordSchema, 
    P <: RecordProperty0](inline p: P): PropertyUpdater[p.type] = 
    val i = indexOfProp(p)
    values => newPropertyValue =>
      val arr = Tuples.toIArray(values)
      val updatedArray = arr.updated(i, newPropertyValue.asInstanceOf[Object])
      Tuples.fromIArray(updatedArray).asInstanceOf[Values]
    
  // TODO: construct a tuple expression that will return result at once, without the need to reconstruct multiple tuples along the way.
  transparent inline def projectorFrom[S1 <: RecordSchema](inline s1: S1): s1.Values => Values =
    val fp = s1.propertyGetter(p) // : s1.Values => RecordProperty0.PropertyValueType[p.type]
    val fschema: s1.Values => schema.Values = schema.projectorFrom(s1)
    (v: s1.Values) => fp(v) *: fschema(v)

  type AppendOtherSchema[S2 <: RecordSchema] = SchemaCons[P, schema.AppendOtherSchema[S2]]
  transparent inline def appendOtherSchema[S2 <: RecordSchema](inline s2: S2): AppendOtherSchema[S2] =
    p #: schema.appendOtherSchema(s2)

  transparent inline def replace[P1 <: RecordProperty0, P2 <: RecordProperty0](inline p1: P1, inline p2: P2): RecordSchema =
    inline p1 match
      case `p` => p2 #: schema
      case _   => p #: schema.replace(p1, p2)

  type Remove[P1<:RecordProperty0] <: RecordSchema = 
    P1 match
      case P => S
      case _ => SchemaCons[P, schema.Remove[P1]]

  transparent inline def remove[P1 <: RecordProperty0](inline p1: P1): Remove[p1.type] =
    inline p1 match
      case _: P => schema
      case _    => SchemaCons(p, schema.remove(p1))

  transparent inline def transformOption: OptionValues => Option[Values] =
    val schemaTransformOption = schema.transformOption
    ov => 
      ov match
        case None    *: t => None
        case Some(v) *: t =>
          val tr = schemaTransformOption(t)
          tr.map(v *: _)

  transparent inline def transformEither[E]: EitherValues[E] => Either[List[E], Values] =
    val schemaTransformEither = schema.transformEither[E]
    ev => 
      ev match
        case v1 *: t =>
          val tr = schemaTransformEither(t)
          v1 match 
            case Left(e)  =>
              tr match
                case Left(lst) => Left(e :: lst)
                case Right(_)  => Left(e :: Nil)
            case Right(v) =>
              tr.map(v *: _)

infix type #:[P <: RecordProperty0, S <: RecordSchema] = SchemaCons[P, S]

object RecordSchema:
  type IndexOfTypeInTupleAux[T<:Tuple, A, N <: Int] <: Int = T match
    case EmptyTuple => Nothing
    case A *: t     => N
    case _ *: t     => IndexOfTypeInTupleAux[t, A, S[N]]

  type IndexOfTypeInTuple[T<:Tuple, A] = IndexOfTypeInTupleAux[T, A, 0]

  type Unapply[X <: RecordSchema] = X match
    case EmptySchema      => None.type
    case SchemaCons[p, s] => Some[(p,s)]

  type PropAt[X <: RecordSchema, N <: Int] <: RecordProperty0 = X match
    case EmptySchema      => Nothing
    case SchemaCons[p, s] => 
      N match
        case 0    => p
        case S[n] => PropAt[s, n]

  type ValueAt[X <: RecordSchema, N <: Int] <: Any = X match
    case EmptySchema      => Nothing
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
      case EmptySchema         => Y
      case SchemaCons[x1, xs1] => SchemaCons[x1, Concat[xs1, Y]]
 
  def empty: EmptySchema = EmptySchema

  type TupleToSchema[T <: Tuple] <: RecordSchema =
    T match
      case EmptyTuple => EmptySchema
      case p *: t     => p #: TupleToSchema[t]

  transparent inline def constSchema[S <: RecordSchema]: S =
    inline erasedValue[S] match
      case _: EmptySchema      => RecordSchema.empty.asInstanceOf[S]
      case _: SchemaCons[p, s] =>
        (summonInline[ValueOf[p]].value #: constSchema[s]).asInstanceOf[S]

  transparent inline infix def prepend[S <: RecordSchema, P <: RecordProperty0](inline p: P, inline schema: S): SchemaCons[P, S] =
      SchemaCons[P, S](p, schema)

  type Remove[P1 <: RecordProperty0, S <: RecordSchema] <: RecordSchema =
    S match
      case EmptySchema        => EmptySchema.type
      case SchemaCons[P1, st] => st
      case SchemaCons[pt, st] => SchemaCons[pt, Remove[P1, st]] 

  // DOESN'T WORK
  transparent inline def removeDoesntWork
    [P1 <: RecordProperty0, S <: RecordSchema]
    (inline p1: P1, inline schema: S): Remove[P1, S] =
    inline schema match
      case EmptySchema        : EmptySchema        => EmptySchema
      case SchemaCons(`p1`, s): SchemaCons[P1, st] => s
      case SchemaCons(p,    s): SchemaCons[pt, st] => SchemaCons(p, removeDoesntWork(p1, s)) 

def showExprImpl(a: Expr[Any])(using Quotes): Expr[String] = 
  Expr(a.show)

def to[T, R: Type](f: Expr[T] => Expr[R])(using t: Type[T])(using Quotes): Expr[T => R] =
  '{ (x: t.Underlying) => ${ f('x) } }

def propertyGetterImpl[V <: NonEmptyTuple, I<:Int](i: Expr[Int])(using vt: Type[V], rt: Type[I])(using Quotes): Expr[V => Tuple.Elem[V, I]] = 
  '{
    // import quotes.reflect._
    (v: V) => v.apply($i).asInstanceOf[Tuple.Elem[V, I]]
  }
