Typed ontology meta
===================

This subproject contains 

Traditional web ontology (OWL, RDF) can be considered as a graph 
that is represented with triples. Every node get's it's identifier, 
every link label gets it's id, and all links are just triples `(from, prop, to)`.

In Scala typical form of data is `case class`. This is not very convenient in many cases:

- incomplete data representation

Though there are some benefits:

- type safety
- conveniency
- speed

We want to use type safe triples. 

The core feature of ontology are properties. So we want to be able to declare properties in such a way
that they can be used only with certain types of resources. 

Requirements:

- separate property definitions from data;
- ability to use various data storages;
- case class mapping (including `Option`, `Try` fields);
- ability to define properties from case class using macros;
- ability to use custom classes to describe properties.

One simple consequence (when we consider `Id[_]` to be the same as data container `Id = Record`) is TypedJson.
It's a hierarchical structure resembling Json. However, keys in this structure are typed
properties and "objects" - are typed records `Record[T]`