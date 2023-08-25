import scala.language.implicitConversions


// -> is an implicit method defined in scala.Predef
val pair = "Daniel" -> "555"
val intPair = 1 -> 2


case class Person(name: String) {
  def greet = s"Hi, my name is $name!"
}

implicit def fromStringToPerson(str: String): Person = Person(str)

"Peter".greet // println(fromStringToPerson("Peter").greet)


// implicit parameters
def increment(x: Int)(implicit amount: Int) = x + amount
implicit val defaultAmount: Int = 10

increment(2)  // NOT default args


/*
Implicit scope
- normal scope = LOCAL SCOPE
- imported scope
- companions of all types involved in the method signature
  - List
  - Ordering
  - all the types involved = A or any supertype
*/

case class Purchase(nUnits: Int, unitPrice: Double)
object Purchase {
  implicit val totalPriceOrdering: Ordering[Purchase] =
    Ordering.fromLessThan((a,b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
}

object UnitCountOrdering {
  implicit val unitCountOrdering: Ordering[Purchase] =
    Ordering.fromLessThan(_.nUnits < _.nUnits)
}

object UnitPriceOrdering {
  implicit val unitPriceOrdering: Ordering[Purchase] =
    Ordering.fromLessThan(_.unitPrice < _.unitPrice)
}

// default sorting by total price
List(Purchase(3, 10), Purchase(10, 5), Purchase(2, 20)).sorted

// sort by unitPrice
import UnitPriceOrdering.unitPriceOrdering
List(Purchase(3, 10), Purchase(10, 5), Purchase(2, 20)).sorted

// sort by unitCount explicitly
List(Purchase(3, 10), Purchase(10, 5), Purchase(2, 20)).sorted(UnitCountOrdering.unitCountOrdering)



// Type enrichment (Pimp My Library)
implicit class RichString(string: String) {
  def asInt: Int = Integer.valueOf(string) // java.lang.Integer -> Int
  def encrypt(cypherDistance: Int): String = string.map(c => (c + cypherDistance).asInstanceOf[Char])
}

"3".asInt + 4
"John".encrypt(2)
