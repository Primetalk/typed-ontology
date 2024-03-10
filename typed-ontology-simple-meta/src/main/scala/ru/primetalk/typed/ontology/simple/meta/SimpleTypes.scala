package ru.primetalk.typed.ontology.simple.meta

import scala.annotation.targetName
import java.time.LocalDateTime
import scala.runtime.Tuples
import scala.quoted.Type

/** Provides storage types for ScalarSchema1[T] */
trait ScalarSimpleTypes:
  /** We might have to explicitly limit the list of supported types to avoid ambiguity. Though,
    * using low priority implicits might help.
    */
  type ScalarTypes = AnyVal | String | BigInt | LocalDateTime // Int | String | Boolean | Double
  transparent inline given scalarSchema1svt[
      T <: ScalarTypes,
      S <: ScalarSchema1[T]
  ]: SchemaValueType[S, T] =
    new SchemaValueType[S, T]

trait ConversionSimpleTypes:
  transparent inline given tupleSchemaValueType[S <: TupleSchema](using
      tsvt: TupleSchemaValueType[S]
  ): SchemaValueType[S, tsvt.Value] =
    new SchemaValueType[S, tsvt.Value]

  transparent inline given recordSchemaValueType[S <: RecordSchema](using
      rsvt: RecordSchemaValueType[S]
  ): SchemaValueType[S, rsvt.Value] =
    new SchemaValueType[S, rsvt.Value]

/** Provide storage types (tuples) for tuple schemas.
  */
trait TupleSimpleTypes:

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
trait PropertySimpleTypes:

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

trait RecordSchemaSimpleTypes:
  val a = 0
  // SchemaValueTypes
  transparent inline given emptySchemaSVT: SchemaValueType[EmptySchema.type, EmptyTuple] =
    new SchemaValueType[EmptySchema.type, EmptyTuple]

  transparent inline given emptySchemaRSVT: RecordSchemaValueType[EmptySchema.type] =
    new:
      type Schema = EmptySchema.type
      type Value  = EmptyTuple

  transparent inline given tuple1Schema[VP, P <: SimplePropertyId[?, VP]](using
      svtp: RecordPropertyValueType[P, VP]
  ): RecordSchemaValueType[SchemaCons[P, EmptySchema]] =
    new RecordSchemaValueType[SchemaCons[P, EmptySchema]]:
      type Value = Tuple1[VP]

  transparent inline given nonEmptySchema[
    VP,
    P <: SimplePropertyId[?, VP],
    S <: NonEmptySchema,
    // RS <: P #: S
  ](using
      svtp: RecordPropertyValueType[P, VP],
      svts: RecordSchemaValueType[S]
  ): RecordSchemaValueType[P #: svts.Schema] =
    new:
      type Schema = P #: svts.Schema
      type Value  = VP *: svts.Value

// Projectors
trait ProjectorSimpleTypes extends RecordSchemaSimpleTypes:

  // Projectors for properties
  transparent inline given propertyProjectorHead[
      VP,
      P <: SimplePropertyId[?, VP],
      S <: RecordSchema,
      From <: P #: S,
      VFrom <: Tuple,
  ](using
      rpvt: RecordPropertyValueType[P, VP],
      svtp: SchemaValueType[rpvt.Schema, VP],
      svtps: SchemaValueType[From, VFrom]
  ): PropertyProjector[From, VFrom, P, VP] =

    new:
      val from: SchemaValueType[From, VFrom]       = svtps
      val rpvt: RecordPropertyValueType[P, VP] = rpvt
      type Value = VP
      def apply(v: VFrom): VP =
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
    propertyProjector: PropertyProjector[S, VS, P, VP],
    svtps: SchemaValueType[P2 #: S, VP2 *: VS]
  ): PropertyProjector[P2 #: S, VP2 *: VS, P, VP] =
    new:
      val from: SchemaValueType[P2 #: S, VP2 *: VS] = svtps
      val rpvt: RecordPropertyValueType[P, VP] = rpvt1
      type Value = VP
      
      def apply(v: VP2 *: VS): VP =
        v match
          case _ *: (t: VS) =>
            propertyProjector(t)

  // Projectors for various schemas
  transparent inline given emptySchemaProjector[From <: RecordSchema, VFrom](using
      svt: SchemaValueType[From, VFrom]
  ): Projector[From, VFrom, EmptySchema, EmptyTuple] =
    new:
      val from: SchemaValueType[From, VFrom] = svt
      val to: SchemaValueType[EmptySchema.type, EmptyTuple] =
        emptySchemaSVT
      def apply(v: VFrom): EmptyTuple =
        EmptyTuple

  transparent inline given someSchemaPlusPropertyProjector[
      From <: RecordSchema,
      VFrom,
      P <: SimplePropertyId[?, VP],
      VP,
      S <: RecordSchema,
      VS <: Tuple,
  ](using
      rpvt: RecordPropertyValueType[P, VP],
      existingSchemaProjector: Projector[From, VFrom, S, VS],
      propertyProjector: PropertyProjector[From, VFrom, P, VP],
      svtps: SchemaValueType[P #: S, VP *: VS]
  ): Projector[From, VFrom, P #: S, VP *: VS] =
    new:
      val from: SchemaValueType[From, VFrom] = existingSchemaProjector.from
      val to: SchemaValueType[P #: S, VP *: VS]  = svtps
      def apply(v: VFrom): VP *: VS =
        (propertyProjector(v) *: existingSchemaProjector(v))

  implicit class ValueOps[S <: RecordSchema, V](v: V)(using svtv: SchemaValueType[S, V]):
    type Schema = S
    @targetName("get")
    def /[     
      VP,
      P <: SimplePropertyId[?, VP],
    ](p: P)(using prj: PropertyProjector[S, V, P, VP]): VP =
      prj(v)

trait TableBuilderSimpleTypes:
  final case class TableBuilderExtensionR[S <: RecordSchema](
      schema: S,
      svt: RecordSchemaValueType[S]
  ):
    type Row = svt.Value

  implicit def tableBuilderExtension[T <: TableBuilder](t: T)(using
      svt1: RecordSchemaValueType[t.TableSchema]
  ): TableBuilderExtensionR[t.TableSchema] =
    TableBuilderExtensionR[t.TableSchema](t.tableSchema, svt1)

trait ConcatenatorSimpleTypes extends RecordSchemaSimpleTypes:

  transparent inline given emptyBConcatenator[B <: RecordSchema, VB](using
      SchemaValueType[B, VB]
  ): Concatenator[EmptySchema, EmptyTuple, B, VB, VB] =
    new:
      val aSvt = summon[SchemaValueType[EmptySchema, EmptyTuple]]
      val bSvt = summon[SchemaValueType[B, VB]]

      type Schema = B

      def schemaConcat(a: EmptySchema, b: B): Schema = b
      val abSvt: SchemaValueType[B, VB]              = bSvt

      def apply(a: EmptyTuple, b: VB): VB =
        b

  transparent inline given nonEmptyConcatenator[
      VP,
      P <: SimplePropertyId[?, VP],
      S <: RecordSchema,
      VS <: Tuple,
      B <: RecordSchema,
      VB <: Tuple,
      VPSB <: Tuple
  ](using
      svtS: SchemaValueType[S, VS],
      svtA: SchemaValueType[P #: S, VP *: VS],
      svtB: SchemaValueType[B, VB],
      concatSB: Concatenator[S, VS, B, VB, VPSB],
      svt: SchemaValueType[SchemaCons[P, concatSB.Schema], VP *: VPSB]
  ): Concatenator[P #: S, VP *: VS, B, VB, VP *: VPSB] =
    new:
      val aSvt = svtA
      val bSvt = svtB

      type Schema = SchemaCons[P, concatSB.Schema]

      def schemaConcat(a: P #: S, b: B): Schema =
        a.appendOtherSchema(b)

      val abSvt: SchemaValueType[SchemaCons[P, concatSB.Schema], VP *: VPSB] = svt

      def apply(a: VP *: VS, b: VB): VP *: VPSB =
        Tuples.concat(a, b).asInstanceOf[abSvt.Value]

object SimpleTypes
    extends ScalarSimpleTypes
    with ConversionSimpleTypes
    // with TupleSimpleTypes
    with PropertySimpleTypes
    with RecordSchemaSimpleTypes
    with ProjectorSimpleTypes
    with ConcatenatorSimpleTypes
//with TableBuilderSimpleTypes
