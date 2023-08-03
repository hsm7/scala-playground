
// Infinite numbers stream starting from `n`
def from(n: Int): LazyList[Int] = n #:: from(n + 1)

val nat = from(0) // stream of all natural numbers
val mul3 = nat.filter(_ % 3 == 0) // stream of multiples of 3

// an ancient method to generate prime numbers
def sieve(lx: LazyList[Int]): LazyList[Int] =
  lx.head #:: sieve(lx.filter(_ % lx.head != 0))

println(sieve(from(2)).take(100).toList)