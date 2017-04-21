package ru.primetalk.typed.ontology

import ru.primetalk.typed.ontology.metameta.{SchemaBuilder, MetaSeq, Record}
import ru.primetalk.typed.ontology.metameta.SimplePropertiesMeta.property

/**
  * This object contains phantom types and properties for the domain model.
  */
object ontology {
  import java.time.LocalDate

  /* Example domain model.

     Contains a few entities with properties.
   */

  /** Phantom types that represent inheritance relationship between entities. */
  sealed trait DomainEntity

  abstract final class Person extends DomainEntity

  abstract final class Address extends DomainEntity

  abstract final class Product extends DomainEntity

  abstract final class Order extends DomainEntity

  abstract final class OrderItem extends DomainEntity


  /** Simple type that is used as is. */
  sealed trait Quantity

  case class CountableQuantity(count: Int) extends Quantity

  case class MeasurableQuantity(amount: Double) extends Quantity



  object person extends SchemaBuilder[Person] {
    val name = property[String]
    val address = property[Record[Address]]
    val dob = property[LocalDate]
  }

  object address extends SchemaBuilder[Address] {
    val postalIndex = property[String]
    val street = property[String]
  }

  object product extends SchemaBuilder[Product] {
    val id = property[String]
    val description = property[String]
  }

  object orderItem extends SchemaBuilder[OrderItem] {
    val product = property[Record[Product]]
    val quantity = property[Quantity]
  }

  object order extends SchemaBuilder[Order] {
    val id = property[String]
    val items = property[MetaSeq[Product]]
  }

  /** Captured run time type information. */
  val rtti = {
    import ru.primetalk.typed.ontology.metameta.SimplePropertiesMeta.TypeTagRtti._
    import ru.primetalk.typed.ontology.tmap.TypedMap._
    import scala.reflect.runtime.universe.TypeTag
    val tts = implicitly[TypeTag[String]]
    preserveTypeInfo(
      person.name.captureTypeInfo,
      person.address.captureTypeInfo,
      person.dob.captureTypeInfo,
      address.postalIndex.captureTypeInfo,
      address.street.captureTypeInfo,
      product.id.captureTypeInfo,
      product.description.captureTypeInfo,
      orderItem.product.captureTypeInfo,
      orderItem.quantity.captureTypeInfo,
      order.id.captureTypeInfo,
      order.items.captureTypeInfo
    )
  }
}
