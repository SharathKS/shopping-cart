package com.sc.shop.domain

 /** Carts Items workflow : string -> CartItem --> CartItemWithCheckedPrice **/ 
 case class CartItem(val itemId : String, val quantity : Integer ){
       override def toString =  itemId +" x "+quantity 
 }
 
 sealed trait CartItemWithCheckedPrice //either priced or failed to price
 case class PricedCartItem(val item : CartItem, val price : Double) extends CartItemWithCheckedPrice
 {
      override def toString = item +" -> "+price 
 }
 case class NotPricedCartItem(val item : CartItem, val cause : String) extends CartItemWithCheckedPrice
    
 
/** Basket / Cart Types **/
 class Basket[T](val items : Set[T])
 
 //A Cart have one instance of CartItem per product that has total quantity
 class Cart(items : Set[CartItem]) extends Basket[CartItem](items)
    
 object Cart {
    //build Cart from strings : Apple, Orange, Orange becomes Apple->1, Orange->2
   def fromArrayOfStrings(stringElements : Array[String]) : Cart = {
     new Cart(stringElements
            .groupBy( x => x )
            .mapValues[CartItem](x=> new CartItem(x.head,x.length))
            .values.toSet[CartItem])
   }
 }
 
 //models a Cart in priced state (different from unpriced state, represented by a Cart)
 class PricedCart(items : Set[PricedCartItem], val totalPrice : Double) extends Basket[PricedCartItem](items)
 {
     override def toString = "PricedCart total="+totalPrice +" for items {"+items.mkString(",")+"}" 
 }
  
