package ru.primetalk.typed.ontology.simple.meta

import scala.annotation.targetName
import ru.primetalk.typed.ontology.simplemeta2.RecordProperty0.PropertyValueType
import java.time.LocalDateTime
import scala.runtime.Tuples

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

  transparent inline given propertyValueType[P <: RecordProperty0](using
      vp: ValueOf[P],
      svt: SchemaValueType.Aux1[vp.value.Schema]
  ): RecordPropertyValueType[P] =
    new:
      type Value = svt.Value

trait RecordSchemaSimpleTypes:
  val a = 0
  // SchemaValueTypes
  transparent inline given emptySchemaSVT: SchemaValueType[EmptySchema.type, EmptyTuple] =
    new SchemaValueType[EmptySchema.type, EmptyTuple]

  transparent inline given emptySchemaRSVT: RecordSchemaValueType[EmptySchema.type] =
    new:
      type Schema = EmptySchema.type
      type Value  = EmptyTuple

  transparent inline given tuple1Schema[P <: RecordProperty0](using
      svtp: RecordPropertyValueType[P]
  ): RecordSchemaValueType[SchemaCons[P, EmptySchema]] =
    new RecordSchemaValueType[SchemaCons[P, EmptySchema]]:
      type Value = Tuple1[svtp.Value]

  transparent inline given nonEmptySchema[
      P <: RecordProperty0,
      S <: NonEmptySchema,
      RS <: P #: S
  ](using
      svtp: RecordPropertyValueType[P],
      svts: RecordSchemaValueType[S]
  ): RecordSchemaValueType[RS] =
    new:
      type Schema = RS
      type Value  = svtp.Value *: svts.Value

// Projectors
trait ProjectorSimpleTypes extends RecordSchemaSimpleTypes:

  // Projectors for properties
  transparent inline given propertyProjector[
      P <: RecordProperty0,
      S <: RecordSchema,
      From <: P #: S,
      VFrom,
      VP
  ](using
      vp: ValueOf[P],
      svtp: SchemaValueType[vp.value.Schema, VP],
      svtps: SchemaValueType[From, VFrom]
  ): Projector[From, VFrom, vp.value.Schema, VP] =
    new Projector[From, VFrom, vp.value.Schema, VP]:
      val from: SchemaValueType[From, VFrom]       = svtps
      val to: SchemaValueType[vp.value.Schema, VP] = svtp

      def apply(v: VFrom): VP =
        v match
          case h *: _ =>
            h.asInstanceOf[VP]
          case _ =>
            ???

  transparent inline given propertyProjectorOther[
      P <: RecordProperty0,
      P2 <: RecordProperty0,
      S <: RecordSchema,
      From <: P2 #: S,
      SFrom <: Tuple,
      VP,
      VP2
  ](using
      vp: ValueOf[P],
      proj: Projector[S, SFrom, vp.value.Schema, VP],
      svtps: SchemaValueType[From, VP2 *: SFrom]
  ): Projector[From, VP2 *: SFrom, vp.value.Schema, VP] =
    new:
      val from: SchemaValueType[From, VP2 *: SFrom] = svtps
      val to: SchemaValueType[vp.value.Schema, VP]  = proj.to
      def apply(v: VP2 *: SFrom): VP =
        v match
          case _ *: (t: SFrom) =>
            proj.apply(t)

  // Projectors for various schemas
  transparent inline given emptySchemaProjector[From <: SchemaLike, VFrom](using
      svt: SchemaValueType[From, VFrom]
  ): Projector[From, VFrom, EmptySchema, EmptyTuple] =
    new:
      val from: SchemaValueType[From, VFrom] = svt
      val to: SchemaValueType[EmptySchema.type, EmptyTuple] =
        summon[SchemaValueType[EmptySchema.type, EmptyTuple]](using emptySchemaSVT)
      def apply(v: VFrom): EmptyTuple =
        EmptyTuple

  transparent inline given propertyProjector[
      From <: SchemaLike,
      P <: RecordProperty0,
      S <: RecordSchema,
      To <: P #: S,
      VFrom,
      VP,
      VS <: Tuple
  ](using
      vp: ValueOf[P],
      projs: Projector[From, VFrom, S, VS],
      projp: Projector[From, VFrom, vp.value.Schema, VP],
      svtps: SchemaValueType[To, VP *: VS]
  ): Projector[From, VFrom, To, VP *: VS] =
    new:
      val from: SchemaValueType[From, VFrom] = projs.from
      val to: SchemaValueType[To, VP *: VS]  = svtps
      def apply(v: VFrom): VP *: VS =
        (projp(v) *: projs(v))

  extension [S <: SchemaLike, V](v: V)(using svtv: SchemaValueType[S, V])
    @targetName("get")
    def /[P <: RecordProperty0, VP](p: P)(using prj: Projector[S, V, p.Schema, VP]): VP =
      prj(v)

trait TableBuilderSimpleTypes:
  final case class TableBuilderExtensionR[S <: RecordSchema](
      schema: S,
      svt: RecordSchemaValueType[S]
  ):
    type Row = svt.Value

  implicit def tableBuilderExtension[T <: TableBuilder](t: T)(using
      svt1: RecordSchemaValueType[t.tableSchema.type]
  ): TableBuilderExtensionR[t.tableSchema.type] =
    TableBuilderExtensionR[t.tableSchema.type](t.tableSchema, svt1)

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
      P <: RecordProperty0,
      S <: RecordSchema,
      A <: SchemaCons[P, S],
      B <: RecordSchema,
      VP,
      VS <: Tuple,
      VB <: Tuple,
      VPSB <: Tuple
  ](using
      svtS: SchemaValueType[S, VS],
      svtA: SchemaValueType[A, VP *: VS],
      svtB: SchemaValueType[B, VB],
      concatSB: Concatenator[S, VS, B, VB, VPSB],
      svt: SchemaValueType[SchemaCons[P, concatSB.Schema], VP *: VPSB]
  ): Concatenator[A, VP *: VS, B, VB, VP *: VPSB] =
    new:
      val aSvt = svtA
      val bSvt = svtB

      type Schema = SchemaCons[P, concatSB.Schema]

      def schemaConcat(a: A, b: B): Schema =
        a.appendOtherSchema(b)

      val abSvt: SchemaValueType[SchemaCons[P, concatSB.Schema], VP *: VPSB] = svt

      def apply(a: VP *: VS, b: VB): VP *: VPSB =
        Tuples.concat(a, b).asInstanceOf[abSvt.Value]

object SimpleTypes
    extends PropertySimpleTypes
    with ConversionSimpleTypes
    with TupleSimpleTypes
    with ScalarSimpleTypes
    with RecordSchemaSimpleTypes
    with ProjectorSimpleTypes
    with ConcatenatorSimpleTypes
//with TableBuilderSimpleTypes
