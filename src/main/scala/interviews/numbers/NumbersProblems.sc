import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

// Check if a number is prime
def isPrime(n: Long): Boolean = {
  // O(sqrt(n))
  @tailrec
  def recursion(n: Long, i: Long): Boolean =
    if (i > Math.sqrt(n)) true
    else if (n % i == 0) false
    else recursion(n, i + 1)

  val abs = Math.abs(n)
  if (abs < 2) false
  else recursion(abs, 2)
}

// Decompose a number to its constituent prime divisors
def decompose(n: Long): List[Long] = {

  @tailrec
  def recursion(reminder: Long, primes: List[Long], acc: List[Long]): List[Long] = {
    if (reminder == 1) acc // multiplicative identity
    else if (primes.isEmpty) reminder :: acc // reminder is a prime number > sqrt(n)
    else if (reminder % primes.head == 0) recursion(reminder / primes.head, primes, primes.head :: acc)
    else recursion(reminder, primes.tail, acc)
  }

  assert(n >= 0)
  val primes = (2L to Math.sqrt(n).toLong).filter(isPrime).toList
  recursion(n, primes, List.empty)
}

// Given a list of non-negative numbers, rearrange such that they form the largest possible number.
def largestNumber(numbers: List[Int]): String = {
  val order = new Ordering[String] {
    @tailrec
    override def compare(x: String, y: String): Int =
      if (x.length == 1 && y.length == 1) Ordering[Int].compare(x.toInt, y.toInt)
      else {
        val compareFirstDigits = Ordering[Int].compare(x.take(1).toInt, y.take(1).toInt)
        if (compareFirstDigits != 0) compareFirstDigits
        else if (x.length == 1) compare(x, y.substring(1))
        else if (y.length == 1) compare(x.substring(1), y)
        else compare(x.substring(1), y.substring(1))
      }
  }

  val newOrdering = Ordering.fromLessThan[String]((a, b) => a + b < b + a)

  val number = numbers.map(_.toString).sorted(newOrdering.reverse).mkString("")
  if (numbers.isEmpty || number.take(1) == "0") "0"
  else number
}

// reverse the digits of a number
def reverseDigits(n: Int): Int = {

  @tailrec
  def recursion(rem: Int, acc: Int): Int =
    if (rem == 0) acc // additive inverse
    else {
      val lastDigit = rem % 10
      val result = acc * 10 + lastDigit
      if (result < 0 && acc >= 0) 0
      else recursion(rem / 10, result)
    }

  if (n == Int.MinValue) 0
  else if (n >= 0) recursion(n, 0)
  else -recursion(-n, 0)
}

// parse an Int from a String input with regex
def parseIntRegex(str: String): Int = {
  val pattern = "(\\s)*([+-])?(\\d+)(\\D+)?".r
  val PLUS = "+"
  val MINUS = "-"
  val ZERO = "0"

  val (sign, digits): (String, String) = str match {
    case pattern(_, null, p, _) => ("+", p)
    case pattern(_, s, p, _) => (s, p)
    case _ => (PLUS, ZERO)
  }

  Try((sign + digits).toInt) match {
    case Success(number) => number
    case Failure(_)      => sign match {
      case PLUS  => Int.MaxValue
      case MINUS => Int.MinValue
    }
  }
}

// parse an Int from a String input
// O(n)
@tailrec
def parseIntRecursive(str: String): Int = {

  val WHITESPACE = " "
  val PLUS = "+"
  val MINUS = "-"
  val ZERO = '0'
  val DIGITS = "0123456789".toSet

  @tailrec
  def recursion(rem: String, acc: Int, sign: String): Int = {
    if (rem.isEmpty || !DIGITS.contains(rem.charAt(0))) acc
    else if (acc < 0 && sign == PLUS) Int.MaxValue
    else if (acc >= 0 && sign == MINUS) Int.MinValue
    else {
      val digit = rem.charAt(0) - ZERO
      val result = sign match {
        case PLUS  => 10 * acc + digit
        case MINUS => 10 * acc - digit
      }
      recursion(rem.substring(1), result, sign)
    }
  }

  if (str.startsWith(WHITESPACE)) parseIntRecursive(str.substring(1))
  else if (str.startsWith(PLUS)) recursion(str.substring(1), 0, PLUS)
  else if (str.startsWith(MINUS)) recursion(str.substring(1), 0, MINUS)
  else recursion(str, 0, PLUS)
}

// ugly number, a number with only (2, 3, or 5) factors
@tailrec
def uglyNumber(number: Int): Boolean = {
  if (number == 1) true
  else if (number % 2 == 0) uglyNumber(number / 2)
  else if (number % 3 == 0) uglyNumber(number / 3)
  else if (number % 5 == 0) uglyNumber(number / 5)
  else false
}

// nth ugly number with lazy lists
def nthUglyNumberLazy(i: Int): Int = {
  def from(n: Int): LazyList[Int] = n #:: from(n + 1)
  from(1).filter(uglyNumber).take(i).last
}

// nth ugly number
// O(n) time, O(n)
def nthUglyNumber(index: Int): Int = {

  @tailrec
  def recursion(i: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int = {
    val min = Math.min(q2.head, Math.min(q3.head, q5.head))
    if (i == index) min
    else {
      val newQ2 = if (min == q2.head) q2.tail else q2
      val newQ3 = if (min == q3.head) q3.tail else q3
      val newQ5 = if (min == q5.head) q5.tail else q5
      recursion(i + 1, newQ2.enqueue(2 * min), newQ3.enqueue(3 * min), newQ5.enqueue(5 * min))
    }
  }

  if (index == 1) 1
  else recursion(2, Queue(2), Queue(3), Queue(5))
}

// Find the number which occurs exactly once in the list, all other numbers are duplicates
// O(n) time, O(n) space
def duplicates(list: List[Int]): Int = {

  @tailrec
  def recursion(rem: List[Int], set: Set[Int]): Int = {
    if (rem.isEmpty) set.head
    else if (set.contains(rem.head)) recursion(rem.tail, set - rem.head)
    else recursion(rem.tail, set + rem.head)
  }

  recursion(list, Set.empty)
}

// O(n) time, O(1) space
def fastDuplicates(list: List[Int]): Int = list.foldLeft(0)(_ ^ _)