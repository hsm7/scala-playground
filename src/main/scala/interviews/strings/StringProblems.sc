import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

// count characters
def countCharacters(string: String): Map[Char, Int] = {

  @tailrec
  def countCharsRecursive(rem: String, acc: Map[Char, Int]): Map[Char, Int] =
    if (rem.isEmpty) acc
    else {
      val char = rem.head
      val count = 1 + acc.getOrElse(char, 0)
      val pair = char -> count
      countCharsRecursive(rem.tail, acc + pair)
    }

  def countCharsWithFold(string: String): Map[Char, Int] =
    string.foldLeft(Map.empty[Char, Int]) {
      case (map, char) => map + (char -> (map.getOrElse(char, 0) + 1))
    }

  def countCharsWithGroupBy(string: String): Map[Char, Int] =
    string.groupBy(identity).view.mapValues(_.length).toMap

  def countCharsWithMapReduce(string: String): Map[Char, Int] =
    string.groupMapReduce(identity)(_ => 1)(_ + _)


  countCharsRecursive(string, Map.empty)
}

// check anagrams
def checkAnagrams(a: String, b: String): Boolean = countCharacters(a) == countCharacters(b)
def checkAnagramsWithSort(a: String, b: String): Boolean = a.sorted == b.sorted


// valid parenthesis
def validParenthesis(string: String): Boolean = {

  // O(n) time, O(n) memory
  @tailrec
  def recursion(remaining: String, stack: List[Char]): Boolean = {
    if (remaining.isEmpty) stack.isEmpty
    else if (remaining.head == '(') recursion(remaining.tail, remaining.head :: stack)
    else {
      if (stack.isEmpty) false
      else recursion(remaining.tail, stack.tail)
    }
  }

  // O(n) time, O(1) memory
  @tailrec
  def recursion2(remaining: String, openParenthesis: Int): Boolean = {
    if (remaining.isEmpty) openParenthesis == 0
    else if (remaining.head == '(') recursion2(remaining.tail, openParenthesis + 1)
    else {
      if (openParenthesis == 0) false
      else recursion2(remaining.tail, openParenthesis - 1)
    }
  }

  recursion2(string, 0)
}

// generate valid parenthesis (brute-force)
// O(2^n)
def generateValidParenthesisNaive(n: Int): List[String] = {
  ("()" * n).permutations.filter(validParenthesis).toList
}

// generate valid parenthesis (recursive brute-force)
// O(2^n)
def generateValidParenthesisRecursive(n: Int): List[String] = {

  def recursion(string: String, openParenthesis: Int, count: Int): List[String] = {
    if (openParenthesis < 0) List.empty
    else if (count == 2 * n) {
      if (openParenthesis == 0) List(string)
      else List.empty
    } else {
      val left = recursion(string + "(", openParenthesis + 1, count + 1)
      val right = recursion(string + ")", openParenthesis - 1, count + 1)
      left ++ right
    }
  }

  recursion("(", 1, 1)
}

// generate valid parenthesis (optimal)
// O(m * n^2), m = 2 * n
def generateValidParenthesisOptimal(n: Int): List[String] = {
  @tailrec
  def recursion(count: Int, acc: Set[String]): Set[String] = {
    if (count == n) acc
    else recursion(count + 1, acc.flatMap(injectParentheses))
  }

  def injectParentheses(string: String): Seq[String] =
    for {
      index <- 0 until string.length
    } yield string.take(index) + "()" + string.substring(index)

  if (n < 1) List.empty
  else recursion(1, Set("()")).toList
}

// text justification
def justify(text: String, width: Int): String = {

  // make lines from input words with optimal width <= input width
  @tailrec
  def makeLines(words: List[String], acc: List[String], line: String): List[String] =
    if(words.isEmpty) line :: acc
    else {
      val newLine = line + words.head + " "
      if (newLine.length > width) makeLines(words, line :: acc, "")
      else makeLines(words.tail, acc, newLine)
    }

  // justify a line of words by evenly injecting spaces
  @tailrec
  def justifyLine(line: String, index: Int): String = {
    if (line.length == width) line
    else if (index + 1 == width) justifyLine(line, 0)
    else {
      val newIndex = index + line.substring(index).indexWhere(_ == ' ')
      if (newIndex == index - 1) justifyLine(line, 0)
      else {
        val right = " " + line.substring(newIndex)
        val newLine = line.take(newIndex) + right
        val spaces = right.takeWhile(_ == ' ')
        justifyLine(newLine, newIndex + spaces.length)
      }
    }
  }


  val words = text.split(' ').filterNot(_.isEmpty).toList
  val lines = makeLines(words, List.empty, "")
  val justified = lines.head :: lines.tail.map(line => justifyLine(line, 0))
  justified.reverse.mkString("\n")
}

// ransom note
def ransomNote(note: String, magazine: String): Boolean = {

  @tailrec
  def ransom(words: List[Char], map: Map[Char, Int]): Boolean = {
    if (words.isEmpty) true
    else if (!map.contains(words.head)) false
    else if (map(words.head) == 0) false
    else {
      val entry = words.head -> (map(words.head) - 1)
      ransom(words.tail, map + entry)
    }
  }

  val noteWords = note.split(" ").flatMap(_.toList).toList
  val magazineMap = countCharacters(magazine)
  ransom(noteWords, magazineMap)
}

// compare version numbers
@tailrec
def compareVersions(v1: String, v2: String): Int = {
  val rev1 = v1.takeWhile(_.isDigit).toInt
  val rev2 = v2.takeWhile(_.isDigit).toInt
  val compare = rev1.compare(rev2)
  if (compare != 0) compare
  else {
    val index1 = v1.indexWhere(_ == '.')
    val index2 = v2.indexWhere(_ == '.')
    if (index1 == -1 && index2 == -1) 0
    else if (index1 == -1) compareVersions("0", v2.substring(index2 + 1))
    else if (index2 == -1) compareVersions(v1.substring(index1 + 1), "0")
    else compareVersions(v1.substring(index1 + 1), v2.substring(index2 + 1))
  }
}

// add two numbers represented as strings of arbitrary lengths
def addStrings(a: String, b: String): String = {

  @tailrec
  def recursion(a: String, b: String, carry: String = "", acc: String = "", zeroes: String = ""): String =
    if (a.isEmpty) {
      val total = b.reverse + acc
      if (carry.isEmpty) total
      else recursion(carry.reverse, total.reverse)
    }
    else if (b.isEmpty) {
      val total = a.reverse + acc
      if (carry.isEmpty) total
      else recursion(total.reverse, carry.reverse)
    }
    else {
      val result = a.head - '0' + b.head - '0'
      if (result < 10) recursion(a.tail, b.tail, carry, result + acc, "0" + zeroes)
      else {
        val newCarry = 1 + "0" + zeroes
        val totalCarry = newCarry.take(newCarry.length - carry.length) + carry
        recursion(a.tail, b.tail, totalCarry, result % 10 + acc, "0" + zeroes)
      }
    }

  recursion(a.reverse, b.reverse)
}

// multiply two numbers represented as strings of arbitrary lengths
def multiplyStrings(a: String, b: String): String = {

  def multiply(a: String, b: String): String = {
    val aDigit = a.head - '0'
    val bDigit = b.head - '0'
    val result = aDigit * bDigit
    if (a.tail.nonEmpty && b.tail.nonEmpty) result + a.tail + b.tail
    else if (a.tail.nonEmpty) result + a.tail
    else if (b.tail.nonEmpty) result + b.tail
    else result.toString
  }

  @tailrec
  def factorRecursive(n: String, acc: List[String] = List(), zeros: String = ""): List[String] =
    if (n.isEmpty) acc
    else factorRecursive(n.tail, n.head + zeros :: acc, '0' + zeros)

  def factor(n: String): List[String] = n.zipWithIndex.toList.map {
    case (char, index) => char + "0" * index
  }

  val partialResults = for {
    aUnit <- factor(a.reverse)
    bUnit <- factorRecursive(b.reverse)
  } yield multiply(aUnit, bUnit)

  partialResults.foldLeft("0")(addStrings)
}

// reorganize a string such that no two adjacent chars are identical
// greedy algorithm
def reorganizeStrings(string: String): String = {

  @tailrec
  def recursion(acc: String, forbidden: Char, map: TreeMap[Char, Int]): String = {
    if (map.isEmpty) acc
    else {
      val (char, count) = map.filter(_._1 != forbidden).head
      if (count == 1) recursion(acc + char, char, map - char)
      else recursion(acc + char, char, map + (char -> (count - 1)))
    }
  }

  val charMap = TreeMap.from(countCharacters(string))
  if (charMap.head._2 > (string.length + 1) / 2) ""
  else recursion("", '\u0000', charMap)

}

// reverse the words in a sentence
def reverseWords(string: String): String = string.split(" ").filterNot(_.isEmpty).reverse.mkString(" ")