# Type-class based ontology schema

This project presents an attempt to define schema using Tuples with some evidence that each tuple element has associated type-class instance.

## Column representation

A column should have `Name` at type level. 
This allows to convert to `NamedTuple`s with some usability benefits.

Column type `T` should have `Column[T]` defined.

## Schema representation

Arbitrary tuple such that each element is a column (has `Column[T]`).

## Column value

For a given column type we'll have `ColumnValueType` 
that gives the type of values in that column.

## Record value

We use `ValueWithSchema` as a universal mechanism to associate schema with a value. In particular, Record schema and Record value are associated using `RecordTupleValue` (which is a type alias for `ValueWithSchema`).

Underlying value is in `Tuple`.

## Relations

A relation is a collection of values annotated with the same schema.
We'd like to support the following operations:
- projection 
- cartesian product
- inner join

## Record representation

We have a few options of how to store record values:
- tuples;
- `JsonObject`;
- `Map[String, Any]`;
- ...

In order to generalize storage we might use a type class `RecordRepr` that captures the operations that are required to compose record value and extract column values.

```scala
trait RecordRepr[A]:
  type Prepend[HS, HV, S <:Tuple, A]
  inline def prepend[HS, HV, S <:Tuple](h: HV, value: ValueWithSchema[S, A])(using SchemaValueType[HS *: S, A]): Prepend[HS, HV, S, A] 
  inline def get
```
