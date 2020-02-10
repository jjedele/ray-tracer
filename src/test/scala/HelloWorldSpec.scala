import org.scalatest.{FlatSpec, Matchers}

class HelloWorldSpec extends FlatSpec with Matchers {

  "A HelloWorld object" should "return a proper greeting" in {
    HelloWorld.greeting() should be ("Hello World")
  }

}
