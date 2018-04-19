package uk.gov.hmrc.shoppingcart

import org.scalatest.{FunSuite, Matchers, WordSpec}

class SolutionTest extends WordSpec with Matchers {

  "Step1ShoppingService.checkout" should {
    "return Some(2.05) when input is [Apple, Apple, Orange, Apple]" in {
      val input = List("Apple", "Apple", "Orange", "Apple")
      Solution.Step1ShoppingService.checkout(input) should be(Some(BigDecimal("2.05")))
    }
    "return None when input contains invalid string " in {
      val input = List("Apple", "INVALID", "Orange", "Apple")
      Solution.Step1ShoppingService.checkout(input) should be(None)
    }

    "return Some(0) if input is empty" in {
      val input = List()
      Solution.Step1ShoppingService.checkout(input) should be(Some(0))
    }
  }


}
