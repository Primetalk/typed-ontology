package ru.primetalk.typed.ontology.metameta

/**
  * Value of a property might be a simple one or a record.
  * If it's a simple type, then we use one-to-one mapping [[AnyTypeMappings.anyTypeMapping]].
  * However, for records we may use different representation on different layers of
  * application. So we defer this mapping to a particular data representation.
  */
trait TypeMappings {
  /**
    * Maps type [[A]] to `Type`.
    * This mapping works through implicits.
    */
  sealed trait CompilerTypeMapping[A] {
    type Type
  }

  /**
    * Maps type [[A]] to [[B]].
    *
    * This mapping has the benefit of working in another direction too.
    */
  sealed trait TypeMapping[A,B] extends CompilerTypeMapping[A] {
    type Type = B
  }

  /**
    * We don't need many instances at runtime. All type information
    * is only used by compiler.
    * (Though in some applications we could preserve type information for runtime
    * using some sort of `TypeTag`s.)
    */
  private object TypeMappingImpl extends TypeMapping[Any,Any]

  def typeMapping[A,B]: TypeMapping[A,B] = TypeMappingImpl.asInstanceOf[TypeMapping[A,B]]

  /**
    * One-to-one type mapping.
    * This mapping has the lowest precedence. We can easily override type mappings in descendants.
    */
  trait AnyTypeMappings {
    implicit def anyTypeMapping[T]: TypeMapping[T,T] = typeMapping[T,T]
  }

}
