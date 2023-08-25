
case class User(name: String, age: Int, email: String)


// Part 1 - Type Class Pattern
trait HTMLSerializer[T] {
  def serialize(value: T): String
}

// type class instances (some of which are implicit)
implicit object UserSerializer extends HTMLSerializer[User] {
  override def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email}/> </div>"
}

// 1 - we can define serializers for other  types
implicit object IntSerializer extends HTMLSerializer[Int] {
  override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
}

// 2 - we can define MULTIPLE serializers
object PartialUserSerializer extends HTMLSerializer[User] {
  override def serialize(user: User): String = s"<div>${user.name} </div>"
}

val john = User("John", 32, "john@rockthejvm.com")
UserSerializer.serialize(john)
PartialUserSerializer.serialize(john)

// Part 2 - AD-Hoc Polymorphism with Type Class Instances
object HTMLSerializer {
  def convert[T](value: T)(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  def apply[T](implicit serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
}

HTMLSerializer.convert(42)
HTMLSerializer.convert(john)

// access to the entire type class interface
HTMLSerializer[User].serialize(john)


// Part 3: Implicit Conversions with Implicit Classes and Type Classes
implicit class HTMLEnrichment[T](value: T) {
  def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
}

john.toHTML
7.toHTML


// Part 4: Context Bounds

// Serializer with an implicit parameter
def htmlBoilerplate[T](content: T)(implicit serializer: HTMLSerializer[T]): String =
  s"<html><body> ${content.toHTML(serializer)}</body></html>"


// Serializer with context bounds (Compiler with inject implicit parameter)
def htmlSugar[T : HTMLSerializer](content: T): String = {
  s"<html><body> ${content.toHTML}</body></html>"
}

// implicitly
case class Permissions(mask: String)
implicit val defaultPermissions: Permissions = Permissions("0744")

// in some other part of the  code
val standardPerms = implicitly[Permissions]

def htmlSugarWithImplicitly[T : HTMLSerializer](content: T): String = {
  val serializer = implicitly[HTMLSerializer[T]]
  s"<html><body> ${content.toHTML(serializer)}</body></html>"
}