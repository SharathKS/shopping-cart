package com.sc.shop.domain

import scala.collection.SortedSet

//an offer is price for a given quantity : 1 orange for 1£, 3 orange for 2 £ (buy 3 get one free)
case class ItemOffer(val quantity: Integer, val price: Double)

object ItemOffer {
  
  def fromString(stringRepresentation: String): Option[ItemOffer] = {
    val array = stringRepresentation.split("x")
    array match {
      case Array(quantity, price) => Some(new ItemOffer(quantity.toInt, price.toDouble))
      case _                      => None
    }

  }
} 

//price offers are modelled as map : key is product id and value is a list of offers on the product
case class PriceList(val offers : Map[String, Set[ItemOffer]])
