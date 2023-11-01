import scala.annotation.tailrec
import scala.collection.immutable.Queue

abstract sealed class Tree[+T] {
  // essential tree methods
  def value: T
  def left: Tree[T]
  def right: Tree[T]
  def isEmpty: Boolean
  val size: Int
  def toString: String

  // Eazy Problems
  def isLeaf: Boolean
  def collectLeaves: List[Tree[T]]
  def leafCount: Int

  // Medium Difficulty Problems
  def collectNodes(level: Int): List[Tree[T]]
  def mirror: Tree[T]
  def sameShapeAs[S >: T](that: Tree[S]): Boolean
  def isSymmetrical: Boolean
  def preorder: List[T]
  def inorder: List[T]
  def postorder: List[T]
  def bfs: List[T]
}

case object Nil extends Tree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: Tree[Nothing] = throw new NoSuchElementException
  override def right: Tree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override val size: Int = 0
  override def toString: String = "[]"

  override def isLeaf: Boolean = false
  override def collectLeaves: List[Tree[Nothing]] = List.empty
  override def leafCount: Int = 0

  override def collectNodes(level: Int): List[Tree[Nothing]] = List.empty
  override def mirror: Tree[Nothing] = this
  override def sameShapeAs[S >: Nothing](that: Tree[S]): Boolean = that.isEmpty
  override def isSymmetrical: Boolean = true
  override def preorder: List[Nothing] = List.empty
  override def inorder: List[Nothing] = List.empty
  override def postorder: List[Nothing] = List.empty
  override def bfs: List[Nothing] = List.empty
}

case class Node[+T](override val value: T, override val left: Tree[T], override val right: Tree[T]) extends Tree[T] {
  override def isEmpty: Boolean = false
  override val size: Int = 1 + left.size + right.size
  override def toString: String = s"[$value, ${left.toString}, ${right.toString}]"

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty
  override def collectLeaves: List[Tree[T]] = {

    @tailrec
    def recursion(frontier: List[Tree[T]], leaves: List[Tree[T]]): List[Tree[T]] = {
      if (frontier.isEmpty) leaves
      else if (frontier.head.isEmpty) recursion(frontier.tail, leaves)
      else if (frontier.head.isLeaf) recursion(frontier.tail, frontier.head :: leaves)
      else recursion(frontier.head.left :: frontier.head.right :: frontier.tail, leaves)
    }

    recursion(List(this), List.empty)
  }
  override def leafCount: Int = collectLeaves.length

  override def collectNodes(level: Int): List[Tree[T]] = {

    @tailrec
    def recursion(depth: Int, nodes: List[Tree[T]]): List[Tree[T]] = {
      if (depth == level) nodes
      else {
        val children = for {
          node  <- nodes
          child <- List(node.left, node.right).filterNot(_.isEmpty)
        } yield child

        if (children.isEmpty) List.empty
        else recursion(depth + 1, children)
      }
    }

    if (level < 0) List.empty
    else recursion(0, List(this))
  }
  override def mirror: Tree[T] = {

    @tailrec
    def recursion(frontier: List[Tree[T]], expanded: Set[Tree[T]], explored: List[Tree[T]]): Tree[T] =
      if (frontier.isEmpty) explored.head

      else if (frontier.head.isEmpty || frontier.head.isLeaf)
        recursion(frontier.tail, expanded, frontier.head :: explored)

      else if (expanded.contains(frontier.head)) {
        val leftNode = explored.head
        val rightNode = explored.tail.head
        val node = Node(frontier.head.value, leftNode, rightNode)
        recursion(frontier.tail, expanded, node :: explored.drop(2))

      } else {
        val leftNode = frontier.head.left
        val rightNode = frontier.head.right
        recursion(leftNode :: rightNode :: frontier, expanded + frontier.head, explored)
      }

    recursion(List(this), Set.empty, List.empty)
  }
  override def sameShapeAs[S >: T](that: Tree[S]): Boolean = {

    @tailrec
    def recursion(thisFrontier: List[Tree[T]], thatFrontier: List[Tree[S]]): Boolean = {
      if (thisFrontier.isEmpty) thatFrontier.isEmpty
      else if (thatFrontier.isEmpty) false
      else (thisFrontier.head, thatFrontier.head) match {
        case (Nil, Nil) => recursion(thisFrontier.tail, thatFrontier.tail)
        case (Node(_, ll, lr), Node(_, rl, rr)) => recursion(ll :: lr :: thisFrontier.tail, rl :: rr :: thatFrontier.tail)
        case _ => false
      }
    }

    recursion(List(this), List(that))
  }
  override def isSymmetrical: Boolean = sameShapeAs(this.mirror)
  override def preorder: List[T] = {

    def stackRecursion(tree: Tree[T]): List[T] =
      if (tree.isEmpty) List.empty
      else tree.value :: stackRecursion(tree.left) ++ stackRecursion(tree.right)

    @tailrec
    def recursion(frontier: List[Tree[T]], acc: List[T] = List.empty): List[T] =
      if (frontier.isEmpty) acc.reverse
      else {
        val tree = frontier.head
        if (tree.isEmpty) recursion(frontier.tail, acc)
        else recursion(tree.left :: tree.right :: frontier.tail, tree.value :: acc)
      }

    @tailrec
    def traverse(frontier: List[Tree[T]], visited: Set[Tree[T]] = Set.empty, acc: Queue[T] = Queue.empty): List[T] = {
      if (frontier.isEmpty) acc.toList
      else {
        val tree = frontier.head
        if (tree.isEmpty) traverse(frontier.tail, visited, acc)
        else if (tree.isLeaf || visited.contains(tree)) traverse(frontier.tail, visited - tree, acc :+ tree.value)
        else traverse(tree :: tree.left :: tree.right :: frontier.tail, visited + tree, acc)
      }
    }

    assert(stackRecursion(this) == recursion(List(this)))
    assert(stackRecursion(this) == traverse(List(this)))

    traverse(List(this))
  }

  override def inorder: List[T] = {

    def stackRecursion(tree: Tree[T]): List[T] =
      if (tree.isEmpty) List.empty
      else stackRecursion(tree.left) ++ (tree.value :: stackRecursion(tree.right))

    @tailrec
    def traverse(frontier: List[Tree[T]], visited: Set[Tree[T]] = Set.empty, acc: Queue[T] = Queue.empty): List[T] =
      if (frontier.isEmpty) acc.toList
      else {
        val tree = frontier.head
        if (tree.isEmpty) traverse(frontier.tail, visited, acc)
        else if (tree.isLeaf || visited.contains(tree)) traverse(frontier.tail, visited - tree, acc :+ tree.value)
        else traverse(tree.left :: tree :: tree.right :: frontier.tail, visited + tree, acc)
      }

    assert(stackRecursion(this) == traverse(List(this)))

    traverse(List(this))
  }
  override def postorder: List[T] = {

    def stackRecursion(tree: Tree[T]): List[T] =
      if (tree.isEmpty) List.empty
      else left.postorder ++ right.postorder :+ value

    @tailrec
    def traverse(frontier: List[Tree[T]], visited: Set[Tree[T]] = Set.empty, acc: Queue[T] = Queue.empty): List[T] =
      if (frontier.isEmpty) acc.toList
      else {
        val tree = frontier.head
        if (tree.isEmpty) traverse(frontier.tail, visited, acc)
        else if (tree.isLeaf || visited.contains(tree)) traverse(frontier.tail, visited - tree, acc :+ tree.value)
        else traverse(tree.left :: tree.right :: tree :: frontier.tail, visited + tree, acc)
      }

    assert(stackRecursion(this) == traverse(List(this)))

    traverse(List(this))
  }
  override def bfs: List[T] = {
    @tailrec
    def recursion(frontier: Queue[Tree[T]], acc: Queue[T]): List[T] =
      if (frontier.isEmpty) acc.toList
      else {
        val tree = frontier.head
        if (tree.isEmpty) recursion(frontier.tail, acc)
        else recursion(frontier.tail :+ tree.left :+ tree.right, acc :+ tree.value)
      }

    recursion(Queue(this), Queue.empty)
  }
}

def hasPathSum(tree: Tree[Int], target: Int): Boolean = {

  @tailrec
  def recursion(frontier: List[Tree[Int]], visited: Set[Tree[Int]], sum: Int): Boolean = {
    if (frontier.isEmpty) false
    else if (frontier.head.isEmpty) recursion(frontier.tail, visited, sum)
    else {
      val tree = frontier.head
      val total = sum + tree.value
      if (visited.contains(tree)) recursion(frontier.tail, visited - tree, sum - tree.value)
      else if (tree.isLeaf && total == target) true
      else if (total > target) recursion(frontier.tail, visited, sum)
      else recursion(tree.left :: tree.right :: tree :: frontier.tail, visited + tree, total)
    }
  }
  recursion(List(tree), Set.empty, 0)
}

def pathSum(tree: Tree[Int], target: Int): List[Tree[Int]] = {

  @tailrec
  def recursion(frontier: List[Tree[Int]], visited: Set[Tree[Int]], sum: Int, acc: List[Tree[Int]]): List[Tree[Int]] = {
    if (frontier.isEmpty) List.empty
    else if (frontier.head.isEmpty) recursion(frontier.tail, visited, sum, acc)
    else {
      val tree = frontier.head
      val total = sum + tree.value
      if (visited.contains(tree)) recursion(frontier.tail, visited - tree, sum - tree.value, acc.tail)
      else if (tree.isLeaf && total == target) (tree :: acc).reverse
      else if (total > target) recursion(frontier.tail, visited, sum, acc)
      else recursion(tree.left :: tree.right :: tree :: frontier.tail, visited + tree, total, tree :: acc)
    }
  }
  recursion(List(tree), Set.empty, 0, List.empty)
}

def allSumPaths(tree: Tree[Int], target: Int): Set[List[Int]] = {

  @tailrec
  def recursion(frontier: List[Tree[Int]],
                visited: Set[Tree[Int]],
                sum: Int,
                path: List[Int],
                acc: Set[List[Int]]
               ): Set[List[Int]] = {
    if (frontier.isEmpty) acc
    else if (frontier.head.isEmpty) recursion(frontier.tail, visited, sum, path, acc)
    else {
      val tree = frontier.head
      val total = sum + tree.value
      if (visited.contains(tree)) {
        recursion(frontier.tail, visited - tree, sum - tree.value, path.tail, acc)
      }
      else if (tree.isLeaf && total == target) {
        recursion(frontier.tail, visited, sum, path, acc + (tree.value :: path).reverse)
      }
      else if (total > target) recursion(frontier.tail, visited, sum, path, acc)
      else recursion(tree.left :: tree.right :: tree :: frontier.tail, visited + tree, total, tree.value :: path, acc)
    }
  }
  recursion(List(tree), Set.empty, 0, List.empty, Set.empty)
}