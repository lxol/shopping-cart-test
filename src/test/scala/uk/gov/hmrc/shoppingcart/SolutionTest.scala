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


  "Step2ShoppingService.checkout" should {
    "return Some(0.6) when input is [Apple] " in {
      val input = List("Apple")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.6")))
    }
    "return Some(0.6) for 2 Apples (buy 1 get 1 free offer)" in {
      val input = List("Apple", "Apple")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.6")))
    }
    "return Some(1.8) for 5 Apples  (buy 1 get 1 free offer)" in {
      val input = List.fill(5)("Apple")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("1.8")))
    }
    "return Some(0.25) for 1 Oranges " in {
      val input = List.fill(1)("Orange")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.25")))
    }
    "return Some(0.50) for 3 Oranges " in {
      val input = List.fill(3)("Orange")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.50")))
    }
    "return Some(1.25) for 7 Oranges " in {
      val input = List.fill(7)("Orange")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("1.25")))
    }
    "return Some(3.05) for 7 Oranges and 5 Apples " in {
      val input = List.fill(7)("Orange") ++ List.fill(5)("Apple")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("3.05")))
    }
    "return Some(0) if input is empty" in {
      val input = List()
      Solution.Step2ShoppingService.checkout(input) should be(Some(0))
    }
    "return None when input contains invalid string " in {
      val input = List("Apple", "INVALID", "Orange", "Apple")
      Solution.Step1ShoppingService.checkout(input) should be(None)
    }
    "return  Some(0.20) when input is [Banana] " in {
      val input = List("Banana")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.2")))
    }
    "return  Some(0.20) when input is [Banana, Banana] " in {
      val input = List.fill(2)("Banana")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.2")))
    }
    "return  Some(0.20) when input 3 Banana " in {
      val input = List.fill(3)("Banana")
      Solution.Step2ShoppingService.checkout(input) should be(Some(BigDecimal("0.4")))
    }
  }

  "Step3ShoppingService.checkout" should {
    "return Some(1.2) when input is [Apple, Apple, Banana] " in {
      val input = List("Apple", "Apple", "Banana")
      Solution.Step3ShoppingService.checkout(input) should be(Some(BigDecimal("1.2")))
    }
    "return Some(1.2) when input is [Apple, Apple, Banana, Banana] " in {
      val input = List("Apple", "Apple", "Banana", "Banana")
      Solution.Step3ShoppingService.checkout(input) should be(Some(BigDecimal("1.2")))
    }
    "return Some(0.8) when input is [Apple, Banana, Banana] " in {
      val input = List("Apple",  "Banana", "Banana")
      Solution.Step3ShoppingService.checkout(input) should be(Some(BigDecimal("0.8")))
    }

    "return Some(1.2) when input is [Apple, Apple, Apple,   Banana] " in {
      val input = List("Apple", "Apple",  "Apple", "Banana")
      Solution.Step3ShoppingService.checkout(input) should be(Some(BigDecimal("1.2")))
    }
  }
}
