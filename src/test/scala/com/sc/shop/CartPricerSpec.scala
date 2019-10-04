package com.sc.shop

import org.scalatest.Matchers
import org.scalatest.WordSpecLike

import com.sc.shop.domain.Cart
import com.sc.shop.domain.PriceList
import com.sc.shop.service.CartPricer

class CartPricerSpec extends WordSpecLike 
  with Matchers
{

  val priceListConfig :Map[String, Set[String]] = Map("Orange" -> Set("1x0.25","3x0.5"), "Apple" -> Set("1x0.6","2x0.6"))
  
   val priceList :PriceList = new PriceList( priceListConfig.mapValues { x => CartCheckoutApp.createOffersFromStrings(x) })
  
  "CartPricer" should {
  
    "price empty Cart to Zero" in {
       val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array[String]()), priceList)
       assert(pricedCart.totalPrice == 0)
    }
  
    "price two oranges and one apple" in {
       val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Orange","Apple","Orange")), priceList)
       assert(pricedCart.totalPrice == 1.1)
    }
  
   "price unknown items (one Pear, with associated price zero), and remove it from output Cart" in {
     val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Pear","Orange","Orange")), CartCheckoutApp.loadPrices())
       assert(pricedCart.totalPrice == 0.5)
       assert(pricedCart.items.size == 1 )
    }

   "price two oranges and one apple from config price source" in {
       val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Orange","Apple","Orange")), priceList)
       assert(pricedCart.totalPrice == 1.1)
    }
   
    "price offer : buy one get one free on apples " in {
      val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Apple","Apple")), priceList)
       assert(pricedCart.totalPrice == 0.6)
       val pricedCart2 = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Apple","Apple","Apple")), priceList)
       assert(pricedCart2.totalPrice == 1.2)
    }
    
    "price offer : 3 for price of 2 on oranges " in {
       val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Orange","Orange")), priceList)
       assert(pricedCart.totalPrice == 0.5)
       val pricedCart2 = CartPricer.checkOutCart(Cart.fromArrayOfStrings(Array("Orange","Orange","Orange")), priceList)
       assert(pricedCart2.totalPrice == 0.5)
    }
  
  }
}

