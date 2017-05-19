Typed ontology
==============

Typed ontology is a typed domain model with emphasis on the properties rather than data storage.

Have a look:

    object person extends SchemaBuilder[Person] {
      val name: PropertyId[Record[Person], String] = property[String]
      val address = property[Record[Address]]
      val dob = property[LocalDate]
    }
    

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
