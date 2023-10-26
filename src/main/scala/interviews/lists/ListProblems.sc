import scala.annotation.tailrec
import scala.util.Random

sealed abstract class RList[+T] {

  /** Standard list functions */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = Cons(elem, this)

  /** Easy problems */

  // get element at a given index
  def apply(index: Int): T
  // the size of the list
  def length: Int
  // reverse a list
  def reverse: RList[T]
  // append another list
  def ++[S >: T](that: RList[S]): RList[S]
  // remove an element from the list
  def remove(index: Int): RList[T]
  // the big 3 - map
  def map[S](f: T => S): RList[S]
  // the big 3 - flatMap
  def flatMap[S](f: T => RList[S]): RList[S]
  // the big 3 - filter
  def filter(p: T => Boolean): RList[T]

  /** Medium difficulty problems */

  // run-length encoding
  def rle: RList[(T, Int)]
  // duplicate each element k times in a row
  def duplicateEach(k: Int): RList[T]
  // rotate the list k positions to the left
  def rotate(k: Int): RList[T]
  // random sample of k elements from this list
  def sample(k: Int): RList[T]

}

object RList {
  def apply[T](iterable: Iterable[T]): RList[T] = {
    // O(n)
    @tailrec
    def recursion(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else recursion(remaining.tail, remaining.head :: acc)
    }

    recursion(iterable, RNil).reverse
  }
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = this
  override def ++[S >: Nothing](that: RList[S]): RList[S] = that
  override def remove(index: Int): RList[Nothing] = this
  override def map[S](f: Nothing => S): RList[S] = this
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = this
  override def filter(p: Nothing => Boolean): RList[Nothing] = this

  override def rle: RList[(Nothing, Int)] = this
  override def duplicateEach(k: Int): RList[Nothing] = this
  override def rotate(k: Int) = this
  override def sample(k: Int) = throw new UnsupportedOperationException
}

case class Cons[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }
    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    // O(min(N, index))
    @tailrec
    def recursion(remaining: RList[T], current: Int): T = {
      if (current == index) remaining.head
      else recursion(remaining.tail, current + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else recursion(this, 0)
  }

  override def length: Int = {
    // O(N)
    @tailrec
    def recursion(remaining: RList[T], accumulator: Int): Int = {
      if (remaining.isEmpty) accumulator
      else recursion(remaining.tail, accumulator + 1)
    }

    recursion(this, 0)
  }

  override def reverse: RList[T] = {
    // O(n)
    @tailrec
    def recursion(remaining: RList[T], accumulator: RList[T]): RList[T] =
      if (remaining.isEmpty) accumulator
      else recursion(remaining.tail, remaining.head :: accumulator)

    recursion(this, RNil)
  }

  override def ++[S >: T](that: RList[S]): RList[S] = {
    // O(n)
    @tailrec
    def recursion(remaining: RList[S], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator
      else recursion(remaining.tail, remaining.head :: accumulator)
    }

    recursion(this.reverse, that)
  }

  override def remove(index: Int): RList[T] = {
    // O(n)
    @tailrec
    def recursion(remaining: RList[T], accumulator: RList[T], current: Int): RList[T] = {
      if (remaining.isEmpty) accumulator
      else if (current == index) recursion(remaining.tail, accumulator, current + 1)
      else recursion(remaining.tail, remaining.head :: accumulator, current + 1)
    }
    recursion(this.reverse, RNil, 0)
  }

  override def map[S](f: T => S): RList[S] = {
    // O(z * n), z = time complexity of f
    @tailrec
    def recursion(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator
      else recursion(remaining.tail, f(remaining.head) :: accumulator)
    }

    recursion(this.reverse, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    // O(z * n), z = time complexity of f
    @tailrec
    def recursion(remaining: RList[T], accumulator: RList[S]): RList[S] = {
      if (remaining.isEmpty) accumulator
      else recursion(remaining.tail, f(remaining.head) ++ accumulator)
    }

    recursion(this.reverse, RNil)
  }

  override def filter(p: T => Boolean): RList[T] = {
    // O(n)
    @tailrec
    def recursion(remaining: RList[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator
      else if (p(remaining.head)) recursion(remaining.tail, remaining.head :: accumulator)
      else recursion(remaining.tail, accumulator)
    }

    recursion(this.reverse, RNil)
  }

  override def rle: RList[(T, Int)] = {
    // O(z * n), z is the maximum number of occurrences in the list
    @tailrec
    def recursion(remaining: RList[T], cons: RList[T], accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty) (cons.head, cons.length) :: accumulator
      else cons match {
        case RNil                              => recursion(remaining.tail, remaining.head :: cons, accumulator)
        case Cons(h, _) if h == remaining.head => recursion(remaining.tail, remaining.head :: cons, accumulator)
        case Cons(h, _)                        => recursion(remaining.tail, remaining.head :: RNil, (h, cons.length) :: accumulator)
      }
    }

    @tailrec
    def recursion2(remaining: RList[T], cons: RList[T], accumulator: RList[RList[T]]): RList[(T, Int)] = {
      if (remaining.isEmpty) (cons :: accumulator).map(l => (l.head, l.length))
      else cons match {
        case RNil                              => recursion2(remaining.tail, remaining.head :: RNil, accumulator)
        case Cons(h, _) if h == remaining.head => recursion2(remaining.tail, remaining.head :: cons, accumulator)
        case Cons(_, _)                        => recursion2(remaining.tail, remaining.head :: RNil, cons :: accumulator)
      }
    }

    recursion(this.reverse, RNil, RNil)
  }

  override def duplicateEach(k: Int): RList[T] = {
    // O(k)
    @tailrec
    def duplicate(element: T, count: Int, accumulator: RList[T]): RList[T] =
      if (count == k) accumulator
      else duplicate(element, count + 1, element :: accumulator)

    // O(k * n), k is the number of duplications
    this.flatMap(elem => duplicate(elem, 0, RNil))
  }

  override def rotate(k: Int): RList[T] = {
    // O(n)
    @tailrec
    def recursion(left: RList[T], right: RList[T], i: Int): RList[T] = {
      if (i == k % this.length) left ++ right.reverse
      else recursion(left.tail, left.head :: right, i + 1)
    }

    recursion(this, RNil, 0)
  }

  override def sample(k: Int): RList[T] = {
    val random = new Random(System.currentTimeMillis())
    val length = this.length

    // O(n * k)
    @tailrec
    def recursion(i: Int, accumulator: RList[T]): RList[T] = {
      if (i == k) accumulator
      else recursion(i + 1, this(random.nextInt(length)) :: accumulator)
    }

    // O(n * k)
    def elegant: RList[T] = RList(1 to k)
      .map(_ => random.nextInt(length))
      .map(this(_))

    elegant
  }

}