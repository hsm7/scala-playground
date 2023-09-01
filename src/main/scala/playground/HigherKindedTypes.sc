
// higher-kinded type class
trait Monad[F[_], A] {
  def flatMap[B](f: A => F[B]): F[B]
  def map[B](f: A => B): F[B]
}

implicit class MonadList[A](list: List[A]) extends Monad[List, A] {
  override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
  override def map[B](f: A => B): List[B] = list.map(f)
}

implicit class MonadOption[A](option: Option[A]) extends Monad[Option, A] {
  override def flatMap[B](f: A => Option[B]): Option[B] = option.flatMap(f)
  override def map[B](f: A => B): Option[B] = option.map(f)
}

def multiply[F[_], A, B](ma: Monad[F, A], mb: Monad[F, B]): F[(A, B)] =
  for {
    a <- ma
    b <- mb
  } yield (a, b)


val monadList = new MonadList(List(1,2,3))
monadList.flatMap(x => List(x, x + 1)) // Monad[List, Int] => List[Int]
monadList.map(_ * 2) // Monad[List, Int] => List[Int]


println(multiply(List(1,2), List("a", "b")))
println(multiply(Some(2), Some("scala")))