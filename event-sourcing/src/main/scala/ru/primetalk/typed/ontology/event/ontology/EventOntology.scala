package ru.primetalk.typed.ontology.event.ontology

import ru.primetalk.typed.ontology.typeclass.schema.{Projector, RecordTupleValue, SchemaValueType}
import ru.primetalk.typed.ontology.typeclass.table.TableColumn

class EventOntology[
  CustomAttributesSchema <: Tuple,
  CustomAttributesValue <: Tuple,
  PrimaryKeySchema <: Tuple,
  PrimaryKeyValue <: Tuple,
  EntitySchema <: Tuple,
  EntityValue <: Tuple
](
   customAttributes: CustomAttributesSchema,
   primaryKeySchema: PrimaryKeySchema,
   entitySchema: EntitySchema,
):
  case object id extends TableColumn["id", Int]
  type id = id.type
  case object timestamp extends TableColumn["timestamp", Long]
  type timestamp = timestamp.type

  type TableSchema = id *: timestamp *: Tuple.Concat[CustomAttributesSchema, EntitySchema]
  val tableSchema: TableSchema = id *: timestamp *: (customAttributes ++ entitySchema)

  type CreateEventSchema = id *: timestamp *: Tuple.Concat[CustomAttributesSchema, Tuple.Concat[PrimaryKeySchema, EntitySchema]]

  type State = Map[RecordTupleValue[PrimaryKeySchema, PrimaryKeyValue], RecordTupleValue[EntitySchema, EntityValue]]

  type CreateEventValue = id.Value *: timestamp.Value *: Tuple.Concat[CustomAttributesValue, Tuple.Concat[PrimaryKeyValue, EntityValue]]

  def handleCreateEvent(
                         oldState: State,
                         createEventValue: RecordTupleValue[CreateEventSchema, CreateEventValue])(
                       using pkProjector: Projector[CreateEventSchema, CreateEventValue, PrimaryKeySchema, PrimaryKeyValue],
                       entityProjector: Projector[CreateEventSchema, CreateEventValue, EntitySchema, EntityValue],
  ): State =
    val pk = pkProjector(createEventValue)
    oldState.updatedWith(pk) {
      case Some(_) => ???
      case None => Some(entityProjector(createEventValue))
    }
