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

    def quantityOffer: QuantityOffer
  }

  final case class AppleItem(name: String, cost: Amount, quantityOffer: QuantityOffer) extends Item

  final case class OrangeItem(name: String, cost: Amount, quantityOffer: QuantityOffer) extends Item

  object Item {
    def itemFromString(str: String): Option[Item] =
      str match {
        case "Apple" => Some(AppleItem("Apple", APPLECOST, BuyOneGetOneFree))
        case "Orange" => Some(OrangeItem("Orange", ORANGECOST, ThreeForThePriceOfTwo))
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

  // Step  2

  sealed trait QuantityOffer {
    val buy: Int

    val priceOf: Int
  }

  final case object NoOffer extends QuantityOffer {
    override val buy: Int = 1

    override val priceOf: Int = 1
  }

  final case object BuyOneGetOneFree extends QuantityOffer {
    override val buy: Int = 2

    override val priceOf: Int = 1
  }

  final case object ThreeForThePriceOfTwo extends QuantityOffer {
    override val buy: Int = 3

    override val priceOf: Int = 2
  }

  trait QuantityOfferCalculation {
    def quantityOffer(items: List[Item]): List[Item]
  }

  object Step2ShoppingService extends ShoppingService with QuantityOfferCalculation {
    override def checkout(items: List[String]): Option[Amount] =
      for {
        itemObjects <- Traverse[List].traverse(items)(Item.itemFromString)
      } yield
        quantityOffer(itemObjects).map(_.cost).sum


    override def quantityOffer(items: List[Item]): List[Item] =
      items.groupBy(identity).flatMap(kv => {
        val quotient = kv._2.size / kv._1.quantityOffer.buy
        val remainder = kv._2.size % kv._1.quantityOffer.buy
        List.fill(quotient * kv._1.quantityOffer.priceOf + remainder)(kv._1)
      }).toList

  }

}
