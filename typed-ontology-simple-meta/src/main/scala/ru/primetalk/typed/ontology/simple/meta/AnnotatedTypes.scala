package ru.primetalk.typed.ontology.simple.meta

import scala.annotation.targetName
import java.time.LocalDateTime
import scala.runtime.Tuples
import scala.quoted.Type
import java.time.LocalDate
import java.time.LocalTime

/** Provides storage types for ScalarSchema1[T] */
trait ScalarAnnotatedTypes:
  /** We might have to explicitly limit the list of supported types to avoid ambiguity. Though,
    * using low priority implicits might help.
    */
  type ScalarTypes = AnyVal | String | BigInt | LocalDateTime | LocalDate | LocalTime // Int | String | Boolean | Double
  transparent inline given scalarSchema1svt[
      T <: ScalarTypes,
      S <: ScalarSchema1[T]
  ]: SchemaValueType[S, T #@ S] =
    new SchemaValueType[S, T #@ S]

trait ConversionAnnotatedTypes:
  transparent inline given tupleSchemaValueType[S <: TupleSchema](using
      tsvt: TupleSchemaValueType[S]
  ): SchemaValueType[S, tsvt.Value #@ S] =
    new SchemaValueType[S, tsvt.Value #@ S]

  transparent inline given recordSchemaValueType[S <: RecordSchema, V<: Tuple](using
      rsvt: RecordSchemaValueType[S, V]
  ): SchemaValueType[S, V #@ S] =
    new SchemaValueType[S, V #@ S]

/** Provide storage types (tuples) for tuple schemas.
  */
trait TupleAnnotatedTypes:

  transparent inline given emptyTupleSchemaSvt: TupleSchemaValueType[EmptyTupleSchema.type] =
    new:
      type Schema = EmptyTupleSchema.type
      type Value  = EmptyTuple

  transparent inline given nonEmptyTupleSchema[
      HS <: SchemaLike,
      TS <: TupleSchema,
      ResultS <: NonEmptyTupleSchema[HS, TS]
  ](using
      hs: SchemaValueType.Aux1[HS],
      ts: TupleSchemaValueType[TS]
  ): TupleSchemaValueType[ResultS] =
    new:
      type Schema = ResultS
      type Value  = hs.Value *: ts.Value

/** Converts schema value type to RecordPropertyValueType for properties. */
trait PropertyAnnotatedTypes:

  // transparent inline given propertyValueType[B, P <: SimplePropertyId[?, B]](using
  //     vp: ValueOf[P],
  //     svt: SchemaValueType.Aux1[vp.value.Schema]
  // ): RecordPropertyValueType[P, B] =
  //   new:
  //     val property = vp.value
  //     // type Value = svt.Value

  transparent inline given propertyValueType[B, P <: SimplePropertyId[?, B]](using vp: ValueOf[P]): RecordPropertyValueType[P, B] = 
    ${ SimpleTypesMacro.propertyValueType1Impl[B, P]('vp) }
  
  // transparent inline given propertyValueType2[B, P <: SimplePropertyId[?, B]]: RecordPropertyValueType[P] = 
  //   new:
  //     type Value = B

trait RecordSchemaAnnotatedTypes:
  val a = 0
  // SchemaValueTypes
  transparent inline given emptySchemaSVT: SchemaValueType[EmptySchema, EmptyTuple #@ EmptySchema] =
    new SchemaValueType[EmptySchema.type, EmptyTuple #@ EmptySchema]

  transparent inline given emptySchemaRSVT: RecordSchemaValueType[EmptySchema, EmptyTuple.type] =
    new RecordSchemaValueType

  transparent inline given tuple1Schema[VP, P <: SimplePropertyId[?, VP]](using
      svtp: RecordPropertyValueType[P, VP]
  ): RecordSchemaValueType[SchemaCons[P, EmptySchema], VP *: EmptyTuple] =
    new RecordSchemaValueType

  transparent inline given nonEmptySchema[
    P <: SimplePropertyId[?, VP],
    VP,
    S <: NonEmptySchema,
    VS <: Tuple
  ](using
      svtp: RecordPropertyValueType[P, VP],
      svts: RecordSchemaValueType[S, VS]
  ): RecordSchemaValueType[P #: svts.Schema, VP *: VS] =
    new RecordSchemaValueType

// Projectors
trait ProjectorAnnotatedTypes extends RecordSchemaAnnotatedTypes:

  transparent inline given propertyProjectorForAnnotatedValue[
    P <: SimplePropertyId[?, VP],
    VP,
    From <: RecordSchema,
    VFrom <: Tuple,
  ](using 
    pp: PropertyProjector[From, VFrom, P, VP],
    svt: SchemaValueType[From, VFrom #@ From]
    ): PropertyProjector[From, VFrom #@ From, P, VP] =
    new:
      val from: SchemaValueType[From, VFrom #@ From]       = svt
      val rpvt: RecordPropertyValueType[P, VP] = pp.rpvt
      type Value = VP
      def apply(v: VFrom #@ From): VP =
        pp.apply(v)
      
  // Projectors for properties
  transparent inline given propertyProjectorHead[
    VP,
    P <: SimplePropertyId[?, VP],
    S <: RecordSchema,
    From <: P #: S,
    VFrom <: Tuple,
  ](using
      rpvt: RecordPropertyValueType[P, VP],
      svtp: SchemaValueType[rpvt.Schema, VP #@ rpvt.Schema],
      svtps: SchemaValueType[From, VFrom #@ From]
  ): PropertyProjector[From, VFrom #@ From, P, VP] =

    new:
      val from: SchemaValueType[From, VFrom #@ From]   = svtps
      val rpvt: RecordPropertyValueType[P, VP] = rpvt
      type Value = VP
      def apply(v: VFrom #@ From): VP =
        v match
          case h *: _ =>
            h.asInstanceOf[VP]
          case _ =>
            ???

  transparent inline given propertyProjectorTail[
      VP,
      P <: SimplePropertyId[?, VP],
      VP2,
      P2 <: SimplePropertyId[?, VP2],
      S <: RecordSchema,
      VS <: Tuple,
      // From <: P2 #: S,
  ](using
    rpvt1: RecordPropertyValueType[P, VP],
    propertyProjector: PropertyProjector[S, VS #@ S, P, VP],
    svtps: SchemaValueType[P2 #: S, (VP2 *: VS) #@ (P2 #: S)]
  ): PropertyProjector[P2 #: S, (VP2 *: VS) #@ (P2 #: S), P, VP] =
    new:
      val from: SchemaValueType[P2 #: S, (VP2 *: VS) #@ (P2 #: S)] = svtps
      val rpvt: RecordPropertyValueType[P, VP] = rpvt1
      type Value = VP
      
      def apply(v: (VP2 *: VS) #@ (P2 #: S)): VP =
        v match
          case _ *: (t: VS) =>
            propertyProjector(t.#@[S])

  // Projectors for various schemas
  transparent inline given emptySchemaProjector[From <: RecordSchema, VFrom](using
      svt: SchemaValueType[From, VFrom #@ From]
  ): Projector[From, VFrom #@ From, EmptySchema, EmptyTuple #@ EmptySchema] =
    new:
      val from: SchemaValueType[From, VFrom #@ From] = svt
      val to: SchemaValueType[EmptySchema, EmptyTuple #@ EmptySchema] =
        emptySchemaSVT
      def apply(v: VFrom #@ From): EmptyTuple #@ EmptySchema =
        EmptyTuple.#@[EmptySchema]

  transparent inline given someSchemaPlusPropertyProjector[
      From <: RecordSchema,
      VFrom,
      P <: SimplePropertyId[?, VP],
      VP,
      S <: RecordSchema,
      VS <: Tuple,
  ](using
      rpvt: RecordPropertyValueType[P, VP],
      existingSchemaProjector: Projector[From, VFrom#@From, S, VS #@ S],
      propertyProjector: PropertyProjector[From, VFrom, P, VP],
      svtps: SchemaValueType[P #: S, (VP *: VS) #@ (P #: S)]
  ): Projector[From, VFrom #@From, P #: S, (VP *: VS) #@ (P #: S)] =
    new:
      val from: SchemaValueType[From, VFrom#@From] = existingSchemaProjector.from
      val to: SchemaValueType[P #: S, (VP *: VS) #@ (P #: S)]  = svtps
      def apply(v: VFrom#@From): (VP *: VS) #@ (P #: S) =
        (propertyProjector(v) *: existingSchemaProjector(v)).#@[(P #: S)]

  implicit class ValueOps[S <: RecordSchema, V](v: V)(using svtv: SchemaValueType[S, V]):
    type Schema = S
    @targetName("get")
    def /[     
      VP,
      P <: SimplePropertyId[?, VP],
    ](p: P)(using prj: PropertyProjector[S, V, P, VP]): VP =
      prj(v)

  extension [S <: RecordSchema](s: S)
    def apply[V](v: V)(using svtv: SchemaValueType[S, V #@ S]): V #@ S =
      v.#@[S]

  extension [TB <: TableBuilder](t: TB)
    def row[V](v: V)(using svtv: SchemaValueType[t.TableSchema, V #@ t.TableSchema]): V #@ t.TableSchema =
      v.#@[t.TableSchema]

  extension [S <: RecordSchema, V](av: V #@ S)
    def ->>[VP,P <:SimplePropertyId[?, VP]](p: P)(using pp: PropertyProjector[S, V#@S, P, VP]) = 
      pp.apply(av)
    def /[VP,P <:SimplePropertyId[?, VP]](p: P)(using pp: PropertyProjector[S, V#@S, P, VP]) = 
      pp.apply(av)
    def >>[VS2, S2 <: RecordSchema](s2: S2)(using p: Projector[S, V #@ S, S2, VS2 #@ S2]) =
      p(av)
    inline def π[VS2, S2 <: RecordSchema](s2: S2)(using p: Projector[S, V #@ S, S2, VS2 #@ S2]) =
      p(av)

trait TableBuilderAnnotatedTypes:
  final case class TableBuilderExtensionR[S <: RecordSchema, V <: Tuple](
      schema: S,
      svt: RecordSchemaValueType[S, V]
  ):
    type Row = V

  implicit def tableBuilderExtension[T <: TableBuilder, V <: Tuple](t: T)(using
      svt1: RecordSchemaValueType[t.TableSchema, V]
  ): TableBuilderExtensionR[t.TableSchema, V] =
    TableBuilderExtensionR[t.TableSchema, V](t.tableSchema, svt1)

trait ConcatenatorAnnotatedTypes extends RecordSchemaAnnotatedTypes:

  transparent inline given emptyBConcatenator[B <: RecordSchema, VB](using
      SchemaValueType[B, VB #@ B]
  ): Concatenator[EmptySchema, EmptyTuple #@ EmptySchema, B, VB #@ B, VB #@ B] =
    new:
      val aSvt = summon[SchemaValueType[EmptySchema, EmptyTuple #@ EmptySchema]]
      val bSvt = summon[SchemaValueType[B, VB #@ B]]

      type Schema = B

      def schemaConcat(a: EmptySchema, b: B): Schema = b
      val abSvt: SchemaValueType[B, VB #@ B]              = bSvt

      def apply(a: EmptyTuple #@ EmptySchema, b: VB #@ B): VB #@ B =
        b

  transparent inline given nonEmptyConcatenator[
      VP,
      P <: SimplePropertyId[?, VP],

      S <: RecordSchema,
      VS <: Tuple,

      B <: RecordSchema,
      VB <: Tuple,

      VSB <: Tuple
  ](using
      svtS: SchemaValueType[S, VS #@ S],
      svtA: SchemaValueType[P #: S, (VP *: VS) #@ (P #: S)],
      svtB: SchemaValueType[B, VB #@ B],
      concatSB: Concatenator[S, VS #@ S, B, VB #@ B, VSB #@ (RecordSchema.Concat[S, B])],
      svt: SchemaValueType[P #: concatSB.Schema, (VP *: VSB) #@ (P #: concatSB.Schema)]
  ): Concatenator[P #: S, (VP *: VS) #@ (P #: S), B, VB #@ B, (VP *: VSB) #@ (P #: concatSB.Schema)] =
    new:
      val aSvt = svtA
      val bSvt = svtB

      type Schema = P #: concatSB.Schema

      def schemaConcat(a: P #: S, b: B): Schema =
        a.appendOtherSchema(b)

      val abSvt: SchemaValueType[Schema, (VP *: VSB) #@ Schema] = svt

      def apply(a: (VP *: VS) #@ (P #: S), b: VB #@ B): (VP *: VSB) #@ Schema =
        Tuples.concat(a, b).asInstanceOf[abSvt.Value]

object AnnotatedTypesContext
    extends ScalarAnnotatedTypes
    with ConversionAnnotatedTypes
    // with TupleAnnotatedTypes
    with PropertyAnnotatedTypes
    with RecordSchemaAnnotatedTypes
    with ProjectorAnnotatedTypes
    with ConcatenatorAnnotatedTypes
//with TableBuilderAnnotatedTypes
