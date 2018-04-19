package uk.gov.hmrc.shoppingcart

import scalaz._
import scalaz.std.list._
import scalaz.std.option._

object Solution {

  val ORANGECOST = BigDecimal("0.25")
  val APPLECOST = BigDecimal("0.60")
  type Amount = BigDecimal

  sealed trait Item {
    def name: String

    def cost: Amount
  }

  final case class AppleItem(name: String, cost: Amount) extends Item

  final case class OrangeItem(name: String, cost: Amount) extends Item

  object Item {
    def itemFromString(str: String): Option[Item] =
      str match {
        case "Apple" => Some(AppleItem("Apple", APPLECOST))
        case "Orange" => Some(OrangeItem("Orange", ORANGECOST))
        case _ => None
      }
  }

  trait ShoppingService {
    def checkout(items: List[String]): Option[Amount]
  }

  object Step1ShoppingService extends ShoppingService {
    override def checkout(items: List[String]): Option[Amount] =
      Traverse[List].traverse(items)(Item.itemFromString).map(_.map(_.cost).sum)
  }

}
