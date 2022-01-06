Typed ontology
==============

Typed ontology is a principled approach to model various domains with emphasis on the properties rather than data storage.

We work with the following abstraction layers:
- application - actual code that work with data;
- schema - part of application that describes the data structure, object/properties//table/columns//entities/attributes, relations between entities.
- meta - part of application, that specifies actual instruments that
will be used in schema definition - methods for defining entities, methods for dealing with types, methods for defining attributes and relations; There is also `simple-meta` that shows an example of how to define properties that are identified by names.
- meta-meta - part of typed-ontology library (which could be customized), that provides foundation for `meta`. Mostly - type classes and macroses 
to facilitate meta definition.

## Primer

Have a look:

    object person extends SchemaBuilder[Person]:
      val name: PropertyId[Record[Person], String] = property[String]
      val address = property[Record[Address]]
      val dob = property[LocalDate]

These instances are about some properties of a `Record` that might contain some information about 
an abstract "`Person`" (phantom type). From this definition we can say that `Person` is some type
that might have `name`, `address`, `dob`. And we know that if there are some values for these 
properties the values should be of corresponding types.
 
If we take a look at definitions of `Record`, `Person`:

    abstract final class Record[A]
    abstract final class Person

we immediately see that these classes cannot contain any data. In this example we cannot even 
instantiate them. (Though in other cases we may use instantiatable classes.)
 
Does this prevent us from dealing with the data associated with the properties? No. 
We can use a pair:
 
    case class PropertyValue[A,B](propertyId: Property[Record[A], B], B)

To keep more property values we may have a collection of pairs, or use typed-map

    val alice: TypedMap[Person]
    val name = alice.get(person.name) // : Option[String]

Separation of data structure (schema) from actual data storage allows us to easily use 
alternative data representations. For example, we may represent our data directly
as json (see `typed-ontology-json` subproject).

## Applications

### Missing/Partial/errorneous data representation

Sometimes we have to deal with data that does not fit strict schema. For instance, some fields might 
be missing, or there could have been an error during parsing, or we simply don't have information for those fields yet.

### Event representation

Sometimes we want to derive new generic entities for existing entities. For instance, 
we might want to work with events (created/updated/deleted) for some entities.
And we'd rather not repeat our definition of event for all entities. 
In `updated` event we might only keep information about the fields that were actually changed.

### Form conversion

We may have the same information represented in different formats. 
And we'd rather keep core schema definition in a central place.
We should be able to represent conversion mapping using individual format definitions.
