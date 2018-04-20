package uk.gov.hmrc.shoppingcart

import scalaz._
import scalaz.std.list._
import scalaz.std.option._

object Solution {

  val ORANGECOST = BigDecimal("0.25")
  val APPLECOST = BigDecimal("0.60")
  val BANANACECOST = BigDecimal("0.20")
  type Amount = BigDecimal

  sealed trait ShoppingItem {
    def name: String

    def cost: Amount
  }

  final case class AppleItem(name: String, cost: Amount) extends ShoppingItem

  final case class OrangeItem(name: String, cost: Amount) extends ShoppingItem

  final case class BananaItem(name: String, cost: Amount) extends ShoppingItem

  object ShoppingItem {
    def itemFromString(str: String): Option[ShoppingItem] =
      str match {
        case "Apple" => Some(AppleItem("Apple", APPLECOST))
        case "Orange" => Some(OrangeItem("Orange", ORANGECOST))
        case "Banana" => Some(BananaItem("Banana", BANANACECOST))
        case _ => None
      }

    def getQuantityOffer(shoppingItem: ShoppingItem) =
      shoppingItem match {
        case _: AppleItem => Some(BuyOneGetOneFree)
        case _: OrangeItem => Some(ThreeForThePriceOfTwo)
        case _: BananaItem => Some(BuyOneGetOneFree)
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

  trait GetCheapestFreeCalculation {
    def cal(shoppingItems: List[ShoppingItem]): (List[ShoppingItem], Amount) = {
      val (apples, bananas) = shoppingItems.foldLeft(0, 0) { (acc, x) =>
        x match {
          case _: AppleItem => (acc._1 + 1, acc._2)
          case _: BananaItem => (acc._1, acc._2 + 1)
          case _ => acc
        }
      }
      if (apples >= bananas) {
        val offerPrice = BigDecimal(Integer.toString(bananas)) * APPLECOST
        (shoppingItems.filterNot(_.name == "Apple").filterNot(_.name == "Banana")
          ++ List.fill(apples - bananas)(AppleItem("Apple", APPLECOST)), offerPrice)
      } else {
        val offerPrice = BigDecimal(Integer.toString(apples)) * APPLECOST
        (shoppingItems.filterNot(_.name == "Apple").filterNot(_.name == "Banana")
          ++ List.fill(bananas - apples)(BananaItem("Banana", BANANACECOST)), offerPrice)
      }
    }
  }

  object Step2ShoppingService extends ShoppingService with QuantityOfferCalculation {
    override def checkout(items: List[String]): Option[Amount] =
      for {
        itemObjects <- getShoppingItems(items)
      } yield
        quantityOffer(itemObjects).map(_.cost).sum
  }

  object Step3ShoppingService extends ShoppingService with QuantityOfferCalculation with GetCheapestFreeCalculation {
    override def checkout(items: List[String]): Option[Amount] = {
      for {
        shoppingItems <- getShoppingItems(items)
      } yield {
        val (ls, amount) = cal(shoppingItems)
        val qAmount = quantityOffer(ls).map(_.cost).sum
        amount + qAmount
      }
    }

  }

}
