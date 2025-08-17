# Type-class based ontology schema

This project presents an attempt to define schema using Tuples with some evidence that each tuple element has associated type-class instance.

Basic supported features:

- construct schema by prepending a new column to an existing schema;
- construct schema from the list of columns (using macros);
- ensure that a tuple is compatible with the schema;
- check that one schema belongs to another;
- project one tuple in one schema into another schema (in case the target schema is subset of the original one).

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

## Case class compatibility

It's possible to convert an existing case class with compatible fields to Record:
```scala
case class MyProduct1(id: Int, name: String, price: BigInt)
val p = MyProduct1(1, "product name", 1)
val row = p.asRecord[Product.TableSchema]
val p2 = row.as[MyProduct1]
assert(p == p2)
```
These inline functions use `Mirror` to perform compile time conversion.

