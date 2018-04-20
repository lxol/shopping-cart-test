package uk.gov.hmrc.shoppingcart

import scalaz._
import scalaz.std.list._
import scalaz.std.option._

object Solution {

  val ORANGECOST = BigDecimal("0.25")
  val APPLECOST = BigDecimal("0.60")
  type Amount = BigDecimal

  sealed trait ShoppingItem {
    def name: String

    def cost: Amount
  }

  final case class AppleItem(name: String, cost: Amount) extends ShoppingItem

  final case class OrangeItem(name: String, cost: Amount) extends ShoppingItem

  object ShoppingItem {
    def itemFromString(str: String): Option[ShoppingItem] =
      str match {
        case "Apple" => Some(AppleItem("Apple", APPLECOST))
        case "Orange" => Some(OrangeItem("Orange", ORANGECOST))
        case _ => None
      }

    def getQuantityOffer(shoppingItem: ShoppingItem) =
      shoppingItem match {
        case _: AppleItem => Some(BuyOneGetOneFree)
        case _: OrangeItem => Some(ThreeForThePriceOfTwo)
        case _ => None
      }
  }

  trait ShoppingService {
    def getShoppingItems(items: List[String]): Option[List[ShoppingItem]] =
      Traverse[List].traverse(items)(ShoppingItem.itemFromString)

    def checkout(items: List[String]): Option[Amount]
  }

  object Step1ShoppingService extends ShoppingService {
    override def checkout(items: List[String]): Option[Amount] =
      getShoppingItems(items).map(_.map(_.cost).sum)
  }

  // Step  2

  sealed trait QuantityOffer {
    val buy: Int

    val priceOf: Int
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
    def quantityOffer(shoppingItems: List[ShoppingItem]): List[ShoppingItem] =
      shoppingItems.groupBy(identity).flatMap { kv => {
        val shoppingItem = kv._1
        val shoppingItems = kv._2
        ShoppingItem.getQuantityOffer(shoppingItem).map {
          qOffer => {
            val quotient = shoppingItems.size / qOffer.buy
            val remainder = shoppingItems.size % qOffer.buy
            List.fill(quotient * qOffer.priceOf + remainder)(shoppingItem)
          }
        }.getOrElse(shoppingItems)
      }
      }.toList
  }

  object Step2ShoppingService extends ShoppingService with QuantityOfferCalculation {
    override def checkout(items: List[String]): Option[Amount] =
      for {
        itemObjects <- getShoppingItems(items)
      } yield
        quantityOffer(itemObjects).map(_.cost).sum
  }

}
