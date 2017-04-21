
package ru.primetalk.typed.ontology

import java.time.LocalDate

/* Example domain model.

   Contains a few entities with properties.
 */

sealed trait DomainEntity

case class Person(name: String, address: Address, dob: LocalDate) extends DomainEntity

case class Address(postalIndex: String, street: String) extends DomainEntity

case class Product(id: String, description: String) extends DomainEntity

case class Order(id: String, items: Seq[OrderItem]) extends DomainEntity

case class OrderItem(product: Product, quantity: Quantity) extends DomainEntity

sealed trait Quantity

case class CountableQuantity(count: Int) extends Quantity

case class MeasurableQuantity(amount: Double) extends Quantity
