# shopping-cart-scala
simple shooping cart pricing service for apples and oranges in scala. builds with sbt or maven.

- Inputs are space separated strings from possible values : "Apple" or "Orange".
- the price list is configured in application.conf file; Apples cost 60p and oranges cost 25p.

##Environment set up [tested]##
- Java 1.8.0_25
- scala 2.11.6
- Maven 3.3.3 or Sbt 0.13

##Running instructions ##
the jar executable, run with basket content as main argument : "Apple" or "Orange".

for example :
~\shopping-cart-scala>scala -cp target/cart-0.1.0.jar com.mf.shop.CartCheckoutApp Apple Orange Orange Orange

console output will be :
started with prices : Map(Apple -> 0.6, Orange -> 0.25)
elements in your Cart : Apple,Orange,Orange,Orange
Output : PricedCart total=1.35 for items {Orange x 3 -> 0.75,Apple x 1 -> 0.6}

