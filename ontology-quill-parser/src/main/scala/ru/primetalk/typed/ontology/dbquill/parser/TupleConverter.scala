package ru.primetalk.typed.ontology.dbquill.parser

/**
  * Конвертирует Tuple Scala 3 в обычный Tuple.
  * Это по-идее, должно позволить воспользоваться встроенным декодером quill - GenericDecoder.
  */
type TupleConverter[T <: Tuple] =
  T match
    case a *: EmptyTuple => Tuple1[a]
    case a *: b *: EmptyTuple => (a, b)
    case a *: b *: c *: EmptyTuple => (a, b, c)
    case a *: b *: c *: d *: EmptyTuple => (a, b, c, d)
    case a *: b *: c *: d *: e *: EmptyTuple => (a, b, c, d, e)
    case a *: b *: c *: d *: e *: f *: EmptyTuple => (a, b, c, d, e, f)
  // TODO: convert other 22 tuples or make macro for the same

inline def tupleConverter[T<:Tuple](t: T): TupleConverter[T] =
  inline t match
    case _: (a *: EmptyTuple) => val tt = t.asInstanceOf[a*: EmptyTuple];Tuple1(tt._1)
    case _: (a *: b *: EmptyTuple) => val tt = t.asInstanceOf[a *: b *: EmptyTuple];(tt._1, tt._2)
    case _: (a *: b *: c *: EmptyTuple) => val tt = t.asInstanceOf[a *: b *: c *: EmptyTuple];(tt._1, tt._2, tt._3)
    case _: (a *: b *: c *: d *: EmptyTuple) => val tt = t.asInstanceOf[a *: b *: c *: d *: EmptyTuple];(tt._1, tt._2, tt._3, tt._4)
    case _: (a *: b *: c *: d *: e *: EmptyTuple) => val tt = t.asInstanceOf[a *: b *: c *: d *: e *: EmptyTuple];(tt._1, tt._2, tt._3, tt._4, tt._5)
    case _: (a *: b *: c *: d *: e *: f *: EmptyTuple) => val tt = t.asInstanceOf[a *: b *: c *: d *: e *: f *: EmptyTuple];(tt._1, tt._2, tt._3, tt._4, tt._5, tt._6)
