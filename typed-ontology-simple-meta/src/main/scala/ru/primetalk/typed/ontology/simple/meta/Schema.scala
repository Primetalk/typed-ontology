/** Package `simple.meta` contains definitions that are used to express ontology. For instance, here
  * we define PropertyId class that represent a property description. We might want to put any
  * additional metainformation in it. For example,
  */
package ru.primetalk.typed.ontology.simple.meta

import ru.primetalk.typed.ontology.metameta.OntologyType.Record
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
import scala.NonEmptyTuple

sealed trait SchemaLike:
  /** Имя типа или сам тип, если имя отсутствует. */
  def tpeRepr: String

  override def toString(): String =
    tpeRepr

sealed trait ScalarSchema extends SchemaLike

sealed trait ScalarSchema1[T] extends ScalarSchema

object ScalarSchema:
  val BooleanScalarSchema = summon[ScalarSchema1[Boolean]]
  val IntScalarSchema     = summon[ScalarSchema1[Int]]
  val StringScalarSchema  = summon[ScalarSchema1[String]]

object ScalarSchema1:
  given [T: ClassTag]: ScalarSchema1[T] =
    new ScalarSchema1[T]:
      def tpeRepr: String = summon[ClassTag[T]].runtimeClass.getSimpleName

sealed trait TupleSchema extends SchemaLike:
  type Schemas <: Tuple
  val schemas: Schemas
  def tpeRepr: String = schemas.toString()

case object EmptyTupleSchema extends TupleSchema:
  type Schemas = EmptyTuple
  val schemas: Schemas = EmptyTuple
final case class NonEmptyTupleSchema[HS <: SchemaLike, TS <: TupleSchema](h: HS, t: TS)
    extends TupleSchema:
  type Schemas = h.type *: t.Schemas
  val schemas: Schemas = h *: t.schemas

/** Schema of properties for a record R. We can use tuples and maps to represent instances of this
  * schema.
  */
sealed trait RecordSchema extends SchemaLike:
  self =>
  import RecordSchema._

  type Type = self.type

  type Properties <: Tuple
  val properties: Properties

  def tpeRepr: String =
    properties.toIArray.mkString(", ")

  type PropertySet = Tuple.Union[Properties] & RecordProperty0

  /** A type function that applies a given type function to each property. */
  type PropertiesMap[F[_]] = Tuple.Map[Properties, F]

  type IndexOfProp[P <: RecordProperty0] = RecordSchema.IndexOfTypeInTuple[Properties, P]
  transparent inline def indexOfProp[P2 <: RecordProperty0](inline p2: P2): IndexOfProp[P2] =
    RecordSchema.indexOfProp(this, p2)

  /** Returns the tuple made of indices of properties of another schema.
    */
  type IndicesOfProps[S2 <: RecordSchema] <: Tuple =
    S2 match
      case EmptySchema      => EmptyTuple
      case SchemaCons[p, s] => IndexOfProp[p] *: IndicesOfProps[s]
  transparent inline def indicesOfProps[S2 <: RecordSchema](s2: S2): IndicesOfProps[s2.type] =
    constValueTuple[IndicesOfProps[s2.type]]

  /** Concatenates properties of another schema. */
  // transparent inline def concat[S2 <: RecordSchema, This >: this.type <: RecordSchema](inline schema2: S2): RecordSchema.Concat[This, schema2.type] =
  inline def concat[S2 <: RecordSchema](inline schema2: S2): RecordSchema.Concat[this.type, S2] =
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
  transparent inline def prependOtherSchema[S1 <: RecordSchema](
      inline s1: S1
  ): PrependOtherSchema[S1] =
    inline s1 match
      case _: EmptySchema =>
        this
      case sc: SchemaCons[p, s] =>
        val ps: PrependOtherSchema[s] = prependOtherSchema[s](sc.schema)
        SchemaCons[p, PrependOtherSchema[s]](sc.p, ps)

  /** Type of the concatenation of two schemas. */
  type AppendOtherSchema[S2 <: RecordSchema] <: RecordSchema

  transparent inline def appendOtherSchema[S2 <: RecordSchema](inline s2: S2): AppendOtherSchema[S2]

  @targetName("SchemaCons")
  inline def #:[P <: RecordProperty0, This >: this.type <: RecordSchema](p: P): P #: This =
    SchemaCons[P, This](p, this)

  transparent inline def replace[P1 <: RecordProperty0, P2 <: RecordProperty0](
      inline p1: P1,
      inline p2: P2
  ): RecordSchema

  /** Replaces properties of the same type. A bit more restricted version of replace. */
  transparent inline def rename[T, P1 <: RecordProperty[T], P2 <: RecordProperty[T]](
      inline p1: P1,
      inline p2: P2
  ): RecordSchema =
    replace(p1, p2)

  type Remove[P1 <: RecordProperty0] <: RecordSchema

  transparent inline def remove[P1 <: RecordProperty0](inline p1: P1): Remove[P1]

  // The subsequent elements might eventually be moved from schema.
  // Currently it doesn't seem to work outside of this trait...

  /** Simple tuple representing an instance of this schema. */
  type Values = PropertiesMap[RecordProperty0.PropertyValueType]

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
  transparent inline def getByIndex[I <: Int](inline i: I): Values => ValueAt[I] = v =>
    RecordSchema.valueAt[this.type, I](this, i)(v)

  def getByIndexRuntime(i: Int)(v: Values): Any =
    scala.runtime.Tuples.apply(v.asInstanceOf[NonEmptyTuple], i)

  transparent inline def concatValues[S2 <: RecordSchema](schema2: S2)(
      schema3: RecordSchema.Concat[this.type, S2]
  ): (Values, schema2.Values) => schema3.Values =
    (v1, v2) => (v1 ++ v2).asInstanceOf[schema3.Values]

  transparent inline def prependValues[S1 <: RecordSchema](schema1: S1)(
      schema3: PrependOtherSchema[S1]
  ): (schema1.Values, Values) => schema3.Values =
    (v1, v2) => (v1 ++ v2).asInstanceOf[schema3.Values]

  transparent inline def appendValues[S2 <: RecordSchema](schema2: S2)(
      schema3: AppendOtherSchema[S2]
  ): (Values, schema2.Values) => schema3.Values =
    (v1, v2) => (v1 ++ v2).asInstanceOf[schema3.Values]

  type PropertyGetter[P <: RecordProperty0] =
    RecordSchema.IsPropertyInSchema[P, this.type] match
      case true =>
        Values => RecordProperty0.PropertyValueType[P]
      case false =>
        Values => Nothing

  transparent inline def propertyGetter[This >: this.type <: RecordSchema, P <: RecordProperty0](
      p2: P
  ): RecordSchema.PropertyGetter[Values, p2.type]

  type PropertyGetter2[P <: RecordProperty0] <: Values => RecordProperty0.PropertyValueType[P] =
    this.type match
      case EmptySchema        => Nothing
      case SchemaCons[`P`, _] => Values => RecordProperty0.PropertyValueType[P]
      case SchemaCons[_, s]   => Values => RecordProperty0.PropertyValueType[P]

  // transparent inline def propertyGetter2[P <: RecordProperty0](p: P): PropertyGetter2[p.type] =
  //   inline this match
  //     case _ : EmptySchema => scala.compiletime.error(s"property $p not found")
  //     case sc : SchemaCons[`P`, _] => (v: Values) => sc.v.head.asInstanceOf[RecordProperty0.PropertyValueType[p.type]]
  //     case sc : SchemaCons[_, s] => (v: Values) => v.tail.asInstanceOf[s.Values]

  type PropertyUpdater[P <: RecordProperty0] =
    Values => RecordProperty0.PropertyValueType[P] => Values
  transparent inline def propertyUpdater[This >: this.type <: RecordSchema, P <: RecordProperty0](
      inline p: P
  ): PropertyUpdater[P]

  transparent inline def projectorFrom[S1 <: RecordSchema](s1: S1): s1.Values => Values

  transparent inline def projection[S2 <: RecordSchema](
      inline schema2: S2
  ): Any = // Values => S2#Values =
    schema2.projectorFrom(this)

  type OptionValues = ValuesMap[Option]

  transparent inline def transformOption: OptionValues => Option[Values]

  type EitherValues[E] = ValuesMap[[V] =>> Either[E, V]]

  transparent inline def transformEither[E]: EitherValues[E] => Either[List[E], Values]

  transparent inline def fkPredicate[FK <: ForeignKeyId0](fk: FK): Values => Boolean =
    val l = propertyGetter(fk.left)
    val r = propertyGetter(fk.right)
    row => l(row) == r(row)

  extension (values: Values)
    transparent inline def apply[P <: RecordProperty0](p: P) =
      self.propertyGetter(p)(values)
    transparent inline def updated[P <: RecordProperty0](inline p: P)(
        inline v: RecordProperty0.PropertyValueType[P]
    ) =
      self.propertyUpdater(p)(values)(v)

type EmptySchema = EmptySchema.type

case object EmptySchema extends RecordSchema:

  import RecordSchema._
  type R = Nothing

  type ParentSchemaOrNothing = Nothing
  type Properties            = EmptyTuple
  val properties: Properties = EmptyTuple

  def unapply(e: EmptySchema): true = true

  type AppendOtherSchema[S2 <: RecordSchema] = S2
  transparent inline def appendOtherSchema[S2 <: RecordSchema](
      inline s2: S2
  ): AppendOtherSchema[S2] =
    s2

  transparent inline def replace[P1 <: RecordProperty0, P2 <: RecordProperty0](
      inline p1: P1,
      inline p2: P2
  ): RecordSchema =
    EmptySchema

  type Remove[P1 <: RecordProperty0] = EmptySchema

  transparent inline def remove[P1 <: RecordProperty0](inline p1: P1): Remove[P1] = EmptySchema

  def get[P2](p2: P2)(v: Values): Option[RecordProperty0.PropertyValueType[p2.type]] =
    None
  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] =
    m

  transparent inline def propertyGetter[This >: this.type <: RecordSchema, P <: RecordProperty0](
      p2: P
  ): RecordSchema.PropertyGetter[Values, p2.type] =
    sys.error(s"There is no property getter for $p2 in empty schema")
    // ${propertyGetterImpl[This, this.Values, p2.type]('this, 'p2)}
  // transparent inline def propertyGetter[
  //   This >: this.type <: RecordSchema,
  //   P<: RecordProperty0](p: P): PropertyGetter[p.type] =
  //     sys.error(s"There is no property getter for $p in empty schema")
  transparent inline def propertyUpdater[This >: this.type <: RecordSchema, P <: RecordProperty0](
      inline p: P
  ): PropertyUpdater[P] =
    sys.error(s"There is no property updater for $p in empty schema")

  transparent inline def projectorFrom[S1 <: RecordSchema](s1: S1): s1.Values => Values =
    _ => EmptyTuple
  transparent inline def transformOption: OptionValues => Option[Values] =
    _ => Some(EmptyTuple)

  transparent inline def transformEither[E]: EitherValues[E] => Either[List[E], Values] =
    _ => Right(EmptyTuple)

sealed trait NonEmptySchema extends RecordSchema:
  type Properties <: NonEmptyTuple
  type ValuesElem[I <: Int] = Tuple.Elem[Values, I]
  transparent inline def valueAt[N <: Int](n: N): Values => Tuple.Elem[Values, N] =
    v => v.asInstanceOf[NonEmptyTuple].apply(n).asInstanceOf[Tuple.Elem[Values, N]]

final case class SchemaCons[P <: RecordProperty0, S <: RecordSchema](p: P, schema: S)
    extends NonEmptySchema:
  import RecordSchema._

  type ParentSchemaOrNothing = schema.type
  type Properties            = p.type *: schema.Properties
  type PValue                = RecordProperty0.PropertyValueType[Tuple.Head[Properties]] // p.type
  val properties: Properties                       = p *: schema.properties
  def parentSchemaOrNothing: ParentSchemaOrNothing = schema
  def get[P2](p2: P2)(v: Values): Option[RecordProperty0.PropertyValueType[p2.type]] =
    val head *: (tail: schema.Values) = v
    if p2 == p then Some(head.asInstanceOf[RecordProperty0.PropertyValueType[p2.type]])
    else schema.get(p2)(tail)

  transparent inline def get3(p1: p.type)(v: Values): PValue =
    v.head.asInstanceOf[PValue]

  def convertToMap(v: Values, m: Map[String, Any] = Map()): Map[String, Any] =
    val head *: (tail: schema.Values) = v
    schema.convertToMap(tail, m.updated(p.name, head))

  def unapply[This >: this.type <: SchemaCons[P, S]]: Unapply[This] =
    this match
      // case _: EmptySchema      => None
      case _: SchemaCons[p, s] =>
        Some((p, schema))
  transparent inline def propertyGetter[This >: this.type <: RecordSchema, P <: RecordProperty0](
      p2: P
  ): RecordSchema.PropertyGetter[Values, p2.type] =
    ${ propertyGetterImpl[This, this.Values, p2.type]('this, 'p2) }

  // val i: Int = indexOfProp(p2)
  // (values: Values) =>
  //   val res: Any = values.asInstanceOf[NonEmptyTuple].apply(i)
  //   res.asInstanceOf[RecordProperty0.PropertyValueType[p2.type]]////Tuple.Elem[Values, IndexOfProp[p.type]]]
  transparent inline def propertyUpdater[This >: this.type <: RecordSchema, P <: RecordProperty0](
      inline p: P
  ): PropertyUpdater[P] =
    val i = indexOfProp(p)
    values =>
      newPropertyValue =>
        val arr          = Tuples.toIArray(values)
        val updatedArray = arr.updated(i, newPropertyValue.asInstanceOf[Object])
        Tuples.fromIArray(updatedArray).asInstanceOf[Values]

  // TODO: construct a tuple expression that will return result at once, without the need to reconstruct multiple tuples along the way.
  transparent inline def projectorFrom[S1 <: RecordSchema](s1: S1): s1.Values => Values =
    val fp =
      s1.propertyGetter[S1, p.type](p) // : s1.Values => RecordProperty0.PropertyValueType[p.type]
    val fschema: s1.Values => schema.Values = schema.projectorFrom(s1)
    (v: s1.Values) => fp(v) *: fschema(v)

  type AppendOtherSchema[S2 <: RecordSchema] = SchemaCons[P, schema.AppendOtherSchema[S2]]
  transparent inline def appendOtherSchema[S2 <: RecordSchema](
      inline s2: S2
  ): AppendOtherSchema[S2] =
    p #: schema.appendOtherSchema(s2)

  transparent inline def replace[P1 <: RecordProperty0, P2 <: RecordProperty0](
      inline p1: P1,
      inline p2: P2
  ): RecordSchema =
    inline p1 match
      case `p` => p2 #: schema
      case _   => p #: schema.replace(p1, p2)

  type Remove[P1 <: RecordProperty0] <: RecordSchema =
    P1 match
      case P => S
      case _ => SchemaCons[P, schema.Remove[P1]]

  transparent inline def remove[P1 <: RecordProperty0](inline p1: P1): Remove[P1] =
    inline p1 match
      case _: P => schema
      case _    => SchemaCons(p, schema.remove(p1))

  transparent inline def transformOption: OptionValues => Option[Values] =
    val schemaTransformOption: schema.OptionValues => Option[schema.Values] = schema.transformOption
    ov =>
      ov match
        case None *: t => None
        case Some(v: PValue) *: (t: schema.OptionValues) =>
          val tr = schemaTransformOption(t)
          tr.map(v *: _)
        case _ =>
          ???

  transparent inline def transformEither[E]: EitherValues[E] => Either[List[E], Values] =
    val schemaTransformEither: schema.EitherValues[E] => Either[List[E], schema.Values] =
      schema.transformEither[E]
    ev =>
      ev match
        case v1 *: (t: schema.EitherValues[E]) =>
          val tr = schemaTransformEither(t)
          v1 match
            case Left(e: E) =>
              tr match
                case Left(lst) => Left(e :: lst)
                case Right(_)  => Left(e :: Nil)
            case Right(v: PValue) =>
              tr.map(v *: _)

        case _: EmptyTuple =>
          ???

infix type #:[P <: RecordProperty0, S <: RecordSchema] = SchemaCons[P, S]

object RecordSchema:
  type SimpleProperty[R0, N, S <: SchemaLike] = RecordProperty0 {
    type R = R0
    type P = S
  }

  type RecordSchemaFromTupleSchema[
      R,
      PropertySchemas <: Tuple,
      PropertyNames <: Tuple
  ] <: RecordSchema =
    PropertySchemas match
      case EmptyTuple =>
        PropertyNames match
          case EmptyTuple =>
            EmptySchema
      case h *: t =>
        PropertyNames match
          case hn *: tn =>
            SimpleProperty[R, hn, h] #: RecordSchemaFromTupleSchema[R, t, tn]

  type IndexOfTypeInTupleAux[T <: Tuple, A, N <: Int] <: Int = T match
    case EmptyTuple => Nothing
    case A *: t     => N
    case _ *: t     => IndexOfTypeInTupleAux[t, A, S[N]]

  type IndexOfTypeInTuple[T <: Tuple, A] = IndexOfTypeInTupleAux[T, A, 0]

  type Unapply[X <: RecordSchema] = X match
    case EmptySchema      => None.type
    case SchemaCons[p, s] => Some[(p, s)]

  type PropAt[X <: RecordSchema, N <: Int] <: RecordProperty0 = X match
    case EmptySchema => Nothing
    case SchemaCons[p, s] =>
      N match
        case 0    => p
        case S[n] => PropAt[s, n]

  type ValueAt[X <: RecordSchema, N <: Int] <: Any = X match
    case EmptySchema => Nothing
    case SchemaCons[p, s] =>
      N match
        case 0    => RecordProperty0.PropertyValueType[p]
        case S[n] => ValueAt[s, n]

  transparent inline def valueAt[X <: RecordSchema, I <: Int](
      inline schema: X,
      inline i: I
  ): Any => ValueAt[X, I] = v =>
    scala.runtime.Tuples.apply(v.asInstanceOf[NonEmptyTuple], i).asInstanceOf[ValueAt[X, I]]

  transparent inline def indexOfProp[S1 <: RecordSchema, P <: RecordProperty0](
      schema: S1,
      inline property: P
  ): schema.IndexOfProp[P] =
    constValue[schema.IndexOfProp[P]]

  transparent inline def indicesOfProps[S1 <: RecordSchema, S2 <: RecordSchema](
      s1: S1,
      inline s2: S2
  ): s1.IndicesOfProps[S2] =
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
      case _: EmptySchema => RecordSchema.empty.asInstanceOf[S]
      case _: SchemaCons[p, s] =>
        (summonInline[ValueOf[p]].value #: constSchema[s]).asInstanceOf[S]

  transparent inline infix def prepend[S <: RecordSchema, P <: RecordProperty0](
      inline p: P,
      inline schema: S
  ): SchemaCons[P, S] =
    SchemaCons[P, S](p, schema)

  type Remove[P1 <: RecordProperty0, S <: RecordSchema] <: RecordSchema =
    S match
      case EmptySchema        => EmptySchema.type
      case SchemaCons[P1, st] => st
      case SchemaCons[pt, st] => SchemaCons[pt, Remove[P1, st]]

  type IsPropertyInSchema[P <: RecordProperty0, Schema <: RecordSchema] <: Boolean =
    Schema match
      case EmptySchema        => false
      case SchemaCons[`P`, _] => true
      case SchemaCons[_, s]   => IsPropertyInSchema[P, s]

  type PropertyGetter[Values <: Tuple, P <: RecordProperty0] =
    Values => RecordProperty0.PropertyValueType[P]
  // Schema match
  //   case EmptySchema          => EmptyTuple => Nothing
  //   case SchemaCons[`P`, s] => `P` *: s#Values => RecordProperty0.PropertyValueType[P]
  //   case SchemaCons[_, s]     => PropertyGetter[s, P]

  // // DOESN'T WORK
  // transparent inline def removeDoesntWork
  //   [P1 <: RecordProperty0, S <: RecordSchema]
  //   (inline p1: P1, inline schema: S): Remove[P1, S] =
  //   inline schema match
  //     case EmptySchema        : EmptySchema        => EmptySchema
  //     case SchemaCons(`p1`, s): SchemaCons[P1, st] => s
  //     case SchemaCons(p,    s): SchemaCons[pt, st] => SchemaCons(p, removeDoesntWork(p1, s))

  type Reverse0[S <: RecordSchema, Accum <: RecordSchema] <: RecordSchema = S match
    case EmptySchema => Accum
    case SchemaCons[p, s] =>
      Reverse0[s, SchemaCons[p, Accum]]

  type Reverse[S <: RecordSchema] = Reverse0[S, EmptySchema]

  transparent inline def reverse0[S <: RecordSchema, Accum <: RecordSchema](
      s: S,
      accum: Accum
  ): Reverse0[S, Accum] =
    s match
      case _: EmptySchema => accum
      case sc: SchemaCons[p, s] =>
        reverse0[s, SchemaCons[p, Accum]](sc.schema, SchemaCons(sc.p, accum))

  transparent inline def reverse[S <: RecordSchema](s: S): Reverse[S] =
    reverse0(s, EmptySchema)

def showExprImpl(a: Expr[Any])(using Quotes): Expr[String] =
  Expr(a.show)

def to[T, R: Type](f: Expr[T] => Expr[R])(using t: Type[T])(using Quotes): Expr[T => R] =
  '{ (x: t.Underlying) => ${ f('x) } }

transparent inline def isPropertyInSchema[P <: RecordProperty0, Schema <: RecordSchema](
    inline p: P,
    inline schema: Schema
): RecordSchema.IsPropertyInSchema[P, Schema] =
  inline schema match
    case _: EmptySchema        => false
    case _: SchemaCons[`P`, _] => true
    case sc: SchemaCons[_, s]  => isPropertyInSchema[P, s](p, sc.schema)

// def isPropertyInSchemaImpl[P <: RecordProperty0, Schema <: RecordSchema](
//   schema: Expr[Schema], p2: Expr[P]
//   )(using pt: Type[P], schemat: Type[Schema])(using Quotes): Expr[RecordSchema.IsPropertyInSchema[P, Schema]] =
//     import quotes.reflect.*
//     '{
//       ${schemat match
//         case
//       }
//     }
def propertyGetterImpl[Schema <: RecordSchema, Values <: Tuple, P <: RecordProperty0](
    schema: Expr[Schema],
    p2: Expr[P]
)(using pt: Type[P], schemat: Type[Schema], values: Type[Values])(using
    Quotes
): Expr[RecordSchema.PropertyGetter[Values, P]] =
  '{
    val propName = s"prop:${($p2)}"
    if isPropertyInSchema($p2, $schema) then
      val i: Int = $schema.indexOfProp($p2).asInstanceOf[Int]
      val f = (v: Values) =>
        (Tuples
          .apply(v.asInstanceOf[NonEmptyTuple], i)
          .asInstanceOf[RecordProperty0.PropertyValueType[P]])
      f.asInstanceOf[RecordSchema.PropertyGetter[Values, P]]
    else sys.error(s"property $propName is not in schema ${${ schema }}")
  }
