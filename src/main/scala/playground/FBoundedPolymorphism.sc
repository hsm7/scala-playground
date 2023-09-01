
// recursive type: F-Bounded Polymorphism
trait Animal[A <: Animal[A]] {
  def breed: List[Animal[A]]
}

class Cat extends Animal[Cat] {
  override def breed: List[Animal[Cat]] = ??? // List[Cat] !!
}

class Dog extends Animal[Dog] {
  override def breed: List[Dog] = ??? // List[Dog] !!
}

trait Entity[E <: Entity[E]] // ORM
class Person extends Comparable[Person] { // FBP
  override def compareTo(o: Person): Int = ???
}


// pure type classes
trait Animal[A] {
  def breed(a: A): List[A]
}

class Dog
object Dog {
  implicit object DogAnimal extends Animal[Dog] {
    override def breed(a: Dog): List[Dog] = List()
  }
}

class Cat
object Cat {
  implicit object CatAnimal extends Animal[Dog] {
    override def breed(a: Dog): List[Dog] = List()
  }
}

implicit class AnimalOps[A](animal: A) {
  def breed(implicit animalTypeClassInstance: Animal[A]): List[A] =
    animalTypeClassInstance.breed(animal)
}

val dog = new Dog
dog.breed