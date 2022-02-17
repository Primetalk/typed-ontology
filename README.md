Typed ontology
==============

Typed ontology is a principled approach to model various domains with emphasis on the properties rather than data storage.

We work with the following abstraction layers:
- **application** - actual code that work with data;
- **schema** - part of an application that describes the data structure, object/properties//table/columns//entities/attributes, relations between entities.
schema + data together makes up an "ontology". 
However, within this library we consider only schema to be an ontology. 
The data ("instances") is usually a different thing.
This same level is sometimes referred to as "metadata", but we reserve this term and only use "ontology".
- **meta** - part of application, that specifies actual instruments that
will be used in schema definition - methods for defining entities/classes, methods for dealing with types, methods for defining attributes/properties and relations;
  - **simple-meta** project that shows an example of how to define properties that are identified by names. For many applications this simple meta might be enough.
  - For some applications it's required to preserve additional information about properties, for example, database types or serialization/deserialization attributes.
- **meta-tools** - part of typed-ontology library (which could be customized), that provides foundation for `meta`. Mostly - base classes, type classes, macroses to facilitate meta definition.

## Records, attributes and schemas

We use term **record** when we talk about an ontology class or a relational "relation"/"table".
Mainly because the word "class" is already used in the underlying language.

A **record** has **attributes** or **properties** or **columns**. We use these terms interchangingly.
An instance or a **record** may have some **values** of it's **attributes**.
The collection of attributes of an instance is determined by it's **schema**.

For example, an entity `A` may have attributes `a`, `b`, `c`. And we can create a few *schemas* for the same entity - 
`(a,b)`, `(a)`, `(a,b,c)`. We may even talk about a schema that spans a few entities - `(A.a, A.b, B.a)`.

## Primer

Have a look:

```scala
object person extends SchemaBuilder[Person]:
  val name: PropertyId[Record[Person], String] = property[String]
  val address = property[Record[Address]]
  val dob = property[LocalDate]
```
These instances are about some properties of a `Record` that might contain some information about 
an abstract "`Person`" (phantom type). From this definition we can say that `Person` is some type
that might have `name`, `address`, `dob`. And we know that if there are some values for these 
properties the values should be of corresponding types.
 
If we take a look at definitions of `Record`, `Person`:

```scala
abstract final class Record[A]
abstract final class Person
```

we immediately see that these classes cannot contain any data. In this example we cannot even 
instantiate them. (Though in other cases we may use instantiatable classes.)
 
Does this prevent us from dealing with the data associated with the properties? No. 
We can use a pair:
 
```scala
case class PropertyValue[A,B](propertyId: Property[Record[A], B], B)
```

To keep more property values we may have a collection of pairs, or use typed-map

```scala
val alice: TypedMap[Person]
val name = alice.get(person.name) // : Option[String]
```

Alternatively, we can use tuple of values and corresponding tuple of properties.

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

### SQL-style operations

Queries, joins and projections should be possible in typed-ontology.

## Relational algebra

One of the interesting ways of developing ontologies is implementing relational algebra based on ontology.

A **relation** is a **schema** + a collection of instances for that schema:

```
R: <S, V[s]>
```

Particular implementation of storage for instances could be a collection, a stream or something else of kind `* -> *`.

Schema is a tuple of properties:

```
S: <p_i,>
```
Schema `S1` "is a subschema" of another one `S2` if all properties of `S1` are present in `S2`.

Projection is a binary operation:

```
Π: R x S => R
```
This is only defined when the new schema `S` is a subschema of `R.S`.

We may rename some column in a relation if the type of value is the same.

```
rename: S x p => S'
```

Cross product produces all possible combinations of rows in the first relation and in the second one.
The schema of the new relation is a concatenation of the original schemas.

```
R1 x R2 => R{ S = S1 ++ S2, V[s] = V[s1] ** V[s2] }
```
(where `**` is all combinations)

## Tasks

- DONE: fs2-Stream-based relations
- DONE: relational algebra, including projection
- DONE: Table + columns terminology
- DONE: move simplemeta to -meta

Specific relational algebra operations:
-       DONE: projection Π
-       DONE: rename (ρ)
-       DONE: cross product, 
-       DONE: join on foreign key
-       x Natural join (⋈)
        
Collection operations:
-       DONE: set union,
-       DONE: set difference? - via replaceRows
-       DONE: selection σ (filtering)
- DONE: calculate columns
- DONE: groupBy, groupMapReduce
- TODO: sql-style grouping + aggregate (with on-the-fly schema construction)
- TODO: Support case classes (infer schema from case class; map data to case class)
- DONE: calculatable columns based on relational expressions
- TODO: erased relational expressions
- TODO: compile-time relational expressions rewrite for arbitrary expressions

A generic representation of an instance might be a 
- Map[String, Any], 
- a tuple of the appropriate values, 
- a tuple of Options, 
- a tuple of Eithers, … 
Conversion becomes error free, straightforward and fully automated.
