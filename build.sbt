name := """shopping-cart"""

version := "1.0"

scalaVersion := "2.11.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
  
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  libraryDependencies ++= {
  val akkaVersion       = "2.3.11"
  val scalaTestVersion  = "2.2.5"

  Seq(
    "org.scalatest"     %% "scalatest"                            % scalaTestVersion % "test",
    "com.typesafe" % "config" % "1.2.1"
   )
}

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

fork in run := true

