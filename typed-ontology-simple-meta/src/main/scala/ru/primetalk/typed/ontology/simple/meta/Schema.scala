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
/**
  * Schema for simple Scala types.
  */
sealed trait ScalarSchema1[T] extends ScalarSchema

object ScalarSchema1:
  transparent inline given [T: ClassTag]: ScalarSchema1[T] =
    new ScalarSchema1[T]:
      def tpeRepr: String = summon[ClassTag[T]].runtimeClass.getSimpleName
  val BooleanScalarSchema = summon[ScalarSchema1[Boolean]]
  val IntScalarSchema     = summon[ScalarSchema1[Int]]
  val StringScalarSchema  = summon[ScalarSchema1[String]]

sealed trait TupleSchema extends SchemaLike:
  type Schemas <: Tuple
  val schemas: Schemas
  def tpeRepr: String = schemas.toString()

case object EmptyTupleSchema extends TupleSchema:
  type Schemas = EmptyTuple
  val schemas: Schemas = EmptyTuple

sealed trait NonEmptyTupleSchema0 extends TupleSchema

final case class NonEmptyTupleSchema[HS <: SchemaLike, TS <: TupleSchema](h: HS, t: TS)
    extends NonEmptyTupleSchema0:
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

  type PropertySet = RecordProperty0 & Tuple.Fold[Properties, RecordProperty0, [x, y] =>> x & y]

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


  // type Concat1[Y <: RecordSchema] <: RecordSchema =
  //   this.type match
  //     case EmptySchema         => Y
  //     case SchemaCons[x1, xs1] => SchemaCons[x1, Concat[xs1, Y]]

  /** Concatenates properties of another schema. */
  // transparent inline def concat[S2 <: RecordSchema, This >: this.type <: RecordSchema](inline schema2: S2): RecordSchema.Concat[This, schema2.type] =
  // inline def concat[S2 <: RecordSchema](inline schema2: S2): RecordSchema.Concat[this.type, S2] =
  //   inline this match
  //     case _: EmptySchema =>
  //       schema2
  //     case sc: SchemaCons[p, s] =>
  //       sc.p #: sc.schema.concat(schema2)

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
  inline def #:[P <: RecordProperty0, This >: this.type <: RecordSchema](p: P): p.type #: This =
    SchemaCons[p.type, This](p, this)


  transparent inline def ##:[Other <: RecordSchema, This >: this.type <: RecordSchema](inline other: Other) = 
    appendOtherSchema(other)

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

type EmptySchema = EmptySchema.type

case object EmptySchema extends RecordSchema:

  import RecordSchema._
  type R = Nothing

  type ParentSchemaOrNothing = Nothing
  type Properties            = EmptyTuple
  val properties: Properties = EmptyTuple

  def unapply(e: EmptySchema): true = true

  type AppendOtherSchema[S2 <: RecordSchema] = S2

  inline def concat[S2 <: RecordSchema](inline schema2: S2): S2 =
    schema2

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


sealed trait NonEmptySchema extends RecordSchema:
  type Properties <: NonEmptyTuple

final case class SchemaCons[P <: RecordProperty0, S <: RecordSchema](p: P, schema: S)
    extends NonEmptySchema:
  import RecordSchema._

  type ParentSchemaOrNothing = schema.type
  type Properties            = p.type *: schema.Properties
  val properties: Properties                       = p *: schema.properties
  def parentSchemaOrNothing: ParentSchemaOrNothing = schema

  def unapply[This >: this.type <: SchemaCons[P, S]]: Unapply[This] =
    this match
      // case _: EmptySchema      => None
      case _: SchemaCons[p, s] =>
        Some((p, schema))

  type AppendOtherSchema[S2 <: RecordSchema] = SchemaCons[p.type, schema.AppendOtherSchema[S2]]
  transparent inline def appendOtherSchema[S2 <: RecordSchema](
      inline s2: S2
  ): AppendOtherSchema[S2] =
    p #: schema.appendOtherSchema(s2)

  inline def concat[S2 <: RecordSchema](schema2: S2): AppendOtherSchema[schema2.type] =
    appendOtherSchema(schema2)

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

infix type #:[P <: RecordProperty0, S <: RecordSchema] = SchemaCons[P, S]
infix type ##:[S1 <: RecordSchema, S <: RecordSchema] = RecordSchema.Concat[S1, S]

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
