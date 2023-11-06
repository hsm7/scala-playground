import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

type Graph[T] = Map[T, Set[T]]

// Eazy Problems

// number of nodes associated with this node
def outDegree[T](graph: Graph[T], node: T): Int = graph.get(node).map(_.size).getOrElse(0)
// number of nodes connected to this node
def inDegree[T](graph: Graph[T], node: T): Int = graph.count {
  case (_, targets) => targets.contains(node)
}

// Medium Difficulty Problems

// check if there is a path from a source node to a target node
def isPath[T](graph: Graph[T], source: T, target: T): Boolean = {

  @tailrec
  def recursion(frontier: Queue[T], visited: Set[T]): Boolean = {
    if (frontier.isEmpty) false
    else {
      val node = frontier.head
      val neighbors = graph.getOrElse(node, Set.empty)
      if (node == target) true
      else if (visited.contains(node)) recursion(frontier.tail, visited)
      else recursion(frontier.tail ++ neighbors, visited + node)
    }
  }

  recursion(Queue(source), Set.empty)
}

// find a path between a source node and a target node
def findPath[T](graph: Graph[T], source: T, target: T): List[T] = {

  @tailrec
  def recursion(frontier: Queue[T], paths: Queue[List[T]], visited: Set[T]): List[T] = {
    if (frontier.isEmpty) List.empty
    else {
      val node = frontier.head
      val path = paths.head
      val neighbors = graph.getOrElse(node, Set.empty)
      val neighborsPaths = neighbors.map(_ :: path)
      if (node == target) path.reverse
      else if (visited.contains(node)) recursion(frontier.tail, paths.tail, visited)
      else recursion(frontier.tail ++ neighbors, paths.tail ++ neighborsPaths, visited + node)
    }
  }

  recursion(Queue(source), Queue(List(source)), Set.empty)
}

// find all possible paths starting from a source node
def findAllPaths[T](graph: Graph[T], source: T): List[List[T]] = {

  @tailrec
  def recursion(frontier: Queue[T], paths: Queue[List[T]], visited: Set[T], acc: List[List[T]]): List[List[T]] =
    if (frontier.isEmpty) acc.reverse.tail
    else {
      val node = frontier.head
      val path = paths.head
      val neighbors = graph.getOrElse(node, Set.empty)
      val neighborsPaths = neighbors.map(_ :: path)
      if (visited.contains(node)) recursion(frontier.tail, paths.tail, visited, acc)
      else recursion(frontier.tail ++ neighbors, paths.tail ++ neighborsPaths, visited + node, path.reverse :: acc)
    }

  recursion(Queue(source), Queue(List(source)), Set.empty, List.empty)
}

// find a cyclic path from a source node
def findCycle[T](graph: Graph[T], source: T): List[T] = {

  @tailrec
  def recursion(frontier: Queue[T], paths: Queue[List[T]], visited: Set[T]): List[T] =
    if (frontier.isEmpty) List.empty
    else {
      val node = frontier.head
      val path = paths.head
      val neighbors = graph.getOrElse(node, Set.empty)
      val neighborsPaths = neighbors.map(_ :: path)
      if (visited.contains(node)) {
        if (node == source) path.reverse
        else recursion(frontier.tail, paths.tail, visited)
      }
      else recursion(frontier.tail ++ neighbors, paths.tail ++ neighborsPaths, visited + node)
    }

  recursion(Queue(source), Queue(List(source)), Set.empty)
}

// add new add to `graph`
def addEdge[T](graph: Graph[T], source: T, target: T): Graph[T] = {
  val neighbors = graph.getOrElse(source, Set.empty)
  graph + (source -> (neighbors + target))
}

// make `graph` undirected
def makeUndirected[T](graph: Graph[T]): Graph[T] = {

  val nodes = for {
    node     <- graph.keySet
    neighbor <- graph.getOrElse(node, Set.empty)
  } yield neighbor -> node

  nodes.foldLeft(graph) {
    case (newGraph, (neighbor, node)) => addEdge(newGraph, neighbor, node)
  }
}

// the town judge trusts nobody
// everybody trusts the town judge
// there is only one town judge
def findJudge(n: Int, trustList: List[(Int, Int)]): Int = {

  val graph = trustList.foldLeft(Map.empty[Int, Set[Int]]) {
    case (newGraph, (source, target)) => addEdge(newGraph, source, target)
  }

  val judges = (1 to n)
    .filter(judge => outDegree(graph, judge) == 0)
    .filter(judge => inDegree(graph, judge) == n-1)

  if (judges.isEmpty) -1 else judges.head
}

// Hard difficulty problems

// Can you take all courses 0 .. n-1 without breaking any prerequisite?
// check if all dependencies are satisfied
def canTakeAllCourses(n: Int, prerequisites: List[(Int, Int)]): Boolean = {
  val graph = prerequisites.groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
  val courses = (0 until n).toSet

  val prerequisiteCourses = for {
    course       <- graph.keySet if courses.contains(course)
    prerequisite <- graph.getOrElse(course, Set.empty)
  } yield prerequisite

  val graphIsAcyclic = courses.forall(course => findCycle(graph, course).isEmpty)
  val prerequisitesAreSatisfied = prerequisiteCourses.forall(prerequisite => courses.contains(prerequisite))

  graphIsAcyclic && prerequisitesAreSatisfied
}

// find an order that satisfies all prerequisites
def scheduleCoursesWithSorting(n: Int, prerequisites: List[(Int, Int)]): List[Int] = {
  val graph = prerequisites.groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
  val order = (0 until n).sortWith((a, b) => graph.getOrElse(b, Set.empty).contains(a)).toList
  if (canTakeAllCourses(n, prerequisites)) order
  else List.empty
}

// topological sort
// find an order that satisfies all prerequisites
def scheduleCourses(n: Int, prerequisites: List[(Int, Int)]): List[Int] = {
  val emptyGraph = (0 until n).map(_ -> Set[Int]()).toMap
  val graph = emptyGraph ++ prerequisites.groupMapReduce(_._2)(p => Set(p._1))(_ ++ _)

  @tailrec
  def recursion(nodes: Set[Int], frontier: List[Int], expanding: Set[Int],
                visited: Set[Int], order: List[Int]): List[Int] =
    if (nodes.isEmpty) frontier.head :: order
    else if (frontier.isEmpty) recursion(nodes.tail, List(nodes.head), Set.empty, visited, order)
    else {
      val course = frontier.head
      if (visited.contains(course)) recursion(nodes, frontier.tail, expanding, visited, order)
      else if (expanding.contains(course)) {
        recursion(nodes, frontier.tail, expanding - course, visited + course, course :: order)
      } else {
        val courses = graph.getOrElse(course, Set.empty)
        if (courses.exists(c => expanding.contains(c))) List.empty // there is a cycle in graph
        else recursion(nodes, courses.toList ++ frontier, expanding + course, visited, order)
      }
    }

  recursion(graph.keySet, List(), Set.empty, Set.empty, List())
}

// What is the time it takes for the signal to go from the source to ALL the other nodes in the network?
def computeNetworkDelay(n: Int, delays: List[(Int, Int, Int)], source: Int): Int = {
  val graph = delays.groupMapReduce(_._1)(p => Set(p._2 -> p._3))(_ ++ _)

  @tailrec
  def dijkstra(frontier: mutable.PriorityQueue[(Int, Int)], visited: Set[Int], dest: Int): Int =
    if (frontier.isEmpty) Int.MaxValue
    else if (visited.contains(frontier.head._1)) dijkstra(frontier.tail, visited, dest)
    else if (dest == frontier.head._1) frontier.head._2
    else {
      val neighbors = graph.getOrElse(frontier.head._1, Set.empty).map {
        case (neighbor, neighborCost) => neighbor -> (frontier.head._2 + neighborCost)
      }
      dijkstra(frontier ++ neighbors, visited + frontier.head._1, dest)
    }

  (0 until n)
    .map(n => dijkstra(mutable.PriorityQueue(0 -> source)(Ordering.by(_._2)).reverse, Set.empty, n))
    .max
}

// color `graph` such that no adjacent nodes have the same color
def color[T](graph: Graph[T]): Map[T, Int] = {
  val undirected = makeUndirected(graph)

  @tailrec
  def recursion(frontier: List[T], colorings: Map[T, Int], color: Int): Map[T, Int] = {
    if (frontier.isEmpty) colorings
    else if (colorings.contains(frontier.head)) recursion(frontier.tail, colorings, color)
    else {
      val notColored = undirected.keySet -- undirected.getOrElse(frontier.head, Set.empty) -- colorings.keySet
      recursion(frontier.tail, colorings ++ notColored.map(_ -> color), color + 1)
    }
  }

  val ordering = (a: T, b: T) => outDegree(undirected, a) > outDegree(undirected, b)
  val nodesOrdered = undirected.keySet.toList.sortWith(ordering)
  recursion(nodesOrdered, Map.empty, 0)
}

val lotr: Graph[String] = Map(
  "Frodo" -> Set("Aragon", "Sam", "Gollum"),
  "Sam" -> Set("Frodo"),
  "Aragon" -> Set("Frodo", "Gandalf"),
  "Gandalf" -> Set("Frodo", "Sam", "Aragon"),
  "Merry" -> Set("Gandalf")
)

val socialNetwork: Graph[String] = Map(
  "Alice" -> Set("Bob", "Charlie", "David"),
  "Bob" -> Set.empty,
  "Charlie" -> Set("David"),
  "David" -> Set("Bob", "Mary"),
  "Mary" -> Set("Bob", "Charlie")
)

val town: Graph[Int] = Map(
  1 -> Set(4, 5, 2),
  2 -> Set.empty,
  3 -> Set(1, 2),
  4 -> Set(1, 3, 2),
  5 -> Set(2)
)

val courses: Graph[Int] = Map(
  0 -> Set(1, 2, 3),
  1 -> Set.empty,
  2 -> Set(4, 3),
  3 -> Set(4),
  4 -> Set.empty
)

def makeEdgeList(graph: Graph[Int]) = for {
  node <- graph.keySet.toList
  neighbor <- graph.getOrElse(node, Set.empty)
} yield (node, neighbor)

val trustList = makeEdgeList(town)
val prerequisiteList = makeEdgeList(courses)

println(computeNetworkDelay(3, List((0, 1, 3), (0, 2, 6), (1, 2, 2)), 0))
println(computeNetworkDelay(4, List((0, 1, 3), (0, 2, 10), (0, 3, 10), (1, 2, 4), (2, 3, 2)), 0))
println(scheduleCourses(5, prerequisiteList))
println(scheduleCoursesWithSorting(5, prerequisiteList))
println(color(socialNetwork))
println(color(lotr))
