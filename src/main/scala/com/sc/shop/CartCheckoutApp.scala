package com.sc.shop

import java.util.Map.Entry
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaSetConverter
import com.sc.shop.domain.Cart
import com.sc.shop.service.CartPricer
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigValue
import com.sc.shop.domain._

object CartCheckoutApp extends App {
  
  //Load prices from external source
  def loadPrices() : PriceList = 
  {
   //just use hard coded list
   val prices = Map("Orange" -> Set("1x0.25","3x0.5"), "Apple" -> Set("1x0.6","2x0.6"))
    .mapValues( offers => createOffersFromStrings(offers)) 
    
    new PriceList(prices)
  }
  
   lazy val priceList : PriceList = loadPrices()
   
   def createOffersFromStrings(strings : Set[String]) : Set[ItemOffer] ={
     strings.map( x => ItemOffer.fromString(x).get)
   }
  
    Console.println("started with prices : "+priceList)
    Console.println("elements in your Cart : "+args.mkString(","))
    
    val pricedCart = CartPricer.checkOutCart(Cart.fromArrayOfStrings(args), priceList)
    
    Console.println("Output : "+pricedCart)
   
}
