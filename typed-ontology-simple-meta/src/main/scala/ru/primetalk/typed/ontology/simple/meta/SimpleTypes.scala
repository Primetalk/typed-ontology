package ru.primetalk.typed.ontology.simple.meta

import scala.annotation.targetName
import ru.primetalk.typed.ontology.simplemeta2.RecordProperty0.PropertyValueType
import java.time.LocalDateTime

/** Provides storage types for ScalarSchema1[T] */
trait ScalarSimpleTypes:
  /**
    * We might have to explicitly limit the list of supported types to avoid ambiguity.
    * Though, using low priority implicits might help.
    */
  type ScalarTypes = AnyVal | String | BigInt | LocalDateTime //Int | String | Boolean | Double
  transparent inline given scalarSchema1svt[
    T <: ScalarTypes, 
    S <: ScalarSchema1[T]
    ]: SchemaValueType.Aux1[S] =
    new SchemaValueType:
      type Schema = S
      type Value = T


trait ConversionSimpleTypes:
  transparent inline given tupleSchemaValueType[S <: TupleSchema](using tsvt: TupleSchemaValueType[S]): SchemaValueType.Aux1[S] = new SchemaValueType:
    type Schema = S
    type Value = tsvt.Value

  transparent inline given recordSchemaValueType[S <: RecordSchema](using rsvt: RecordSchemaValueType[S]): SchemaValueType.Aux1[S] = new SchemaValueType:
    type Schema = S
    type Value = rsvt.Value

/**
  * Provide storage types (tuples) for tuple schemas.
  */
trait TupleSimpleTypes:
  
  transparent inline given emptyTupleSchemaSvt: TupleSchemaValueType[EmptyTupleSchema.type] = 
    new: 
      type Schema = EmptyTupleSchema.type
      type Value = EmptyTuple

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
      type Value = hs.Value *: ts.Value

/** Converts schema value type to RecordPropertyValueType for properties. */
trait PropertySimpleTypes:
  // transparent inline given propertySvt[P <: RecordProperty0](using
  //     vp: ValueOf[P], svt: SchemaValueType[vp.value.Schema]): SchemaValueType[vp.value.Schema] =
  //   svt


  transparent inline given propertyValueType[P <: RecordProperty0](using
      vp: ValueOf[P], svt: SchemaValueType.Aux1[vp.value.Schema]): RecordPropertyValueType[P] = 
        new:
          type Value = svt.Value

trait RecordSchemaSimpleTypes:
  val a = 0
  // SchemaValueTypes
  transparent inline given emptySchemaSVT: RecordSchemaValueType[EmptySchema.type] = 
    new:
      type Schema = EmptySchema.type
      type Value = EmptyTuple

  transparent inline given tuple1Schema[P <: RecordProperty0](using svtp: RecordPropertyValueType[P]): RecordSchemaValueType[SchemaCons[P, EmptySchema]] = 
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
      type Value = svtp.Value *: svts.Value

// Projectors
trait ProjectorSimpleTypes extends RecordSchemaSimpleTypes:


  // Projectors for properties
  transparent inline given propertyProjector[P <: RecordProperty0, S <: RecordSchema, From <: P #: S](using
      vp: ValueOf[P],
      svtp: SchemaValueType.Aux1[vp.value.Schema],
      svtps: SchemaValueType.Aux1[From]
  ): Projector[From, vp.value.Schema] =
    new:
      val from: SchemaValueType.Aux1[From] = svtps
      val to: SchemaValueType.Aux1[vp.value.Schema]  = svtp
      def apply(v: from.Value): to.Value =
        v match
          case h *: _ =>
            h.asInstanceOf[to.Value]
          case _ => 
            ???

  transparent inline given propertyProjectorOther[
    P <: RecordProperty0, 
    P2 <: RecordProperty0, 
    S <: RecordSchema, 
    From <: P2 #: S](using
      vp: ValueOf[P],
      proj: Projector[S, vp.value.Schema],
      svtps: SchemaValueType.Aux1[From]
  ): Projector[From, vp.value.Schema] =
    new:
      val from: SchemaValueType.Aux1[From] = svtps
      val to: SchemaValueType.Aux1[vp.value.Schema]  = proj.to
      def apply(v: from.Value): to.Value =
        v match
          case _ *: t =>
            proj(t.asInstanceOf[proj.from.Value]).asInstanceOf[to.Value]

  // // Projectors for various schemas
  // transparent inline given emptySchemaProjector[From <: SchemaLike: SchemaValueType]: Projector[From, EmptySchema] =
  //   new:
  //     val from: SchemaValueType[From]      = summon[SchemaValueType[From]]
  //     val to: SchemaValueType[EmptySchema] = summon[SchemaValueType[EmptySchema]]
  //     def apply(v: from.Value): to.Value =
  //       EmptyTuple.asInstanceOf[to.Value]

  transparent inline given [From <: SchemaLike, P <: RecordProperty0, S <: RecordSchema,
  To <: P #: S](using
      vp: ValueOf[P],
      projs: Projector[From, S],
      projp: Projector[From, vp.value.Schema],
      svtps: SchemaValueType.Aux1[To]
  ): Projector[From, To] =
    new:
      val from: SchemaValueType.Aux1[From]  = projs.from
      val to: SchemaValueType.Aux1[To] = svtps
      def apply(v: from.Value): to.Value =
        val t = projs(v.asInstanceOf[projs.from.Value]).asInstanceOf[Tuple]
        (projp(v.asInstanceOf[projp.from.Value]) *: t).asInstanceOf[to.Value]

  extension [V, S <: SchemaLike](v: V)(using svtv: SchemaValueType.Aux[S, V])
    @targetName("get")
    def /[P <: RecordProperty0](p: P)(using prj: Projector[S, p.Schema]): prj.to.Value = 
      prj(v.asInstanceOf[prj.from.Value])

trait TableBuilderSimpleTypes:
  final case class TableBuilderExtensionR[S <: RecordSchema](schema: S, svt: RecordSchemaValueType[S]):
    type Row = svt.Value

  implicit def tableBuilderExtension[T <: TableBuilder](t: T)(using svt1: RecordSchemaValueType[t.tableSchema.type]): TableBuilderExtensionR[t.tableSchema.type] = TableBuilderExtensionR[t.tableSchema.type](t.tableSchema, svt1)

trait ConcatenatorSimpleTypes extends RecordSchemaSimpleTypes:

  transparent inline given emptyBConcatenator[B <: RecordSchema](using RecordSchemaValueType[B]): Concatenator[EmptySchema, B] = 
    new:
      val aSvt = summon[RecordSchemaValueType[EmptySchema]]
      val bSvt = summon[RecordSchemaValueType[B]]

      type Schema = B
      
      def schemaConcat(a: EmptySchema, b: B): Schema = b
      val abSvt: RecordSchemaValueType[Schema] = bSvt

      def apply(a: aSvt.Value, b: bSvt.Value): abSvt.Value = 
        b.asInstanceOf[abSvt.Value]

object SimpleTypes extends PropertySimpleTypes
with ConversionSimpleTypes
with  TupleSimpleTypes 
with ScalarSimpleTypes
with RecordSchemaSimpleTypes
with ProjectorSimpleTypes 
with ConcatenatorSimpleTypes
//with TableBuilderSimpleTypes
