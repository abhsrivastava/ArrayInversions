import scala.io.Source
import Sort._

object CountInversions extends App {
  val data = Source.fromResource("inversions.txt").getLines.map(_.toInt).toList
  val inversions = countInversions(data)
  println(s"number of inversions ${inversions}")

  def countInversions(input: List[Int]): Int = {
    input match {
      case Nil => 0
      case List(a) => 0
      case _ =>
        val mid = input.size / 2
        val left = input.slice(0, mid)
        val right = input.slice(mid, input.size)
        val l1 = countInversions(left)
        val l2 = countInversions(right)
        val l3 = splitInversions(left, right)
        l1 + l2 + l3
    }
  }

  // assuming l1 and l2 are almost of same size.
  // total complexity 2(nlogn + n)
  def splitInversions(l1: List[Int], l2: List[Int]): Int = {
    val sortedL1 = mergeSort(l1) // nlogn
    val sortedL2 = mergeSort(l2) // nlogn
    (sortedL1, sortedL2) match {
      case (Nil, Nil) => 0
      case (Nil, _) => 0
      case (_, Nil) => 0
      case (_, _) if sortedL1.head > sortedL2.head =>
        val result = splitInversions(sortedL1, sortedL2.tail)
        sortedL1.size + result
      case (_, _) =>
        splitInversions(sortedL1.tail, sortedL2)
    }
  }
}