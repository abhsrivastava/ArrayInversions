import scala.io.Source

object ArrayInversions extends App {
    val data = Source.fromResource("inversions.txt").getLines.map(_.toInt).toList
    val inversions = getInversions(data)
    println(s"number of inversions ${inversions.size}")
    def getInversions(input: List[Int]) : List[(Int, Int)] = {
        input match {
            case Nil => Nil
            case List(a) => Nil
            case _ => 
                val mid = input.size / 2
                val left = input.slice(0, mid)
                val right = input.slice(mid, input.size)
                val l1 = getInversions(left)
                val l2 = getInversions(right)
                val l3 = splitInversions(left, right)
                if (l1.nonEmpty && l2.nonEmpty)
                    l1.foldRight(l2)(_ :: _).foldRight(l3)(_ :: _)
                else if (l1.nonEmpty && l2.isEmpty)
                    l1.foldRight(l3)(_ :: _)
                else if (l1.isEmpty && l2.nonEmpty)
                    l2.foldRight(l3)(_ :: _)
                else if (l1.isEmpty && l2.isEmpty)
                    l3
                else {
                    List.empty[(Int, Int)]
                }
        }
    }

    // assuming l1 and l2 are almost of same size.
    // total complexity 2(nlogn + n)
    def splitInversions(l1: List[Int], l2: List[Int]) : List[(Int, Int)] = {
        val sortedL1 = mergeSort(l1) // nlogn
        val sortedL2 = mergeSort(l2) // nlogn
        (sortedL1, sortedL2) match {
            case (Nil, Nil) => List.empty[(Int, Int)]
            case (Nil, _) => List.empty[(Int, Int)]
            case (_, Nil) => List.empty[(Int, Int)]
            case (_, _) if (sortedL1.head > sortedL2.head) => 
                val result = splitInversions(sortedL1, sortedL2.tail)
                sortedL1.foldLeft(result){case (acc, x) => (x, sortedL2.head) :: acc}
            case (_, _) => 
                splitInversions(sortedL1.tail, sortedL2)
        }
    }    

    def mergeSort(input: List[Int]) : List[Int] = {
        input match {
            case Nil => Nil
            case List(a) => List(a)
            case _ => 
                val mid = input.size / 2
                val left = mergeSort(input.slice(0, mid))
                val right = mergeSort(input.slice(mid, input.size))
                merge(left, right)
        }
    }

    def merge(a1: List[Int], a2: List[Int]) : List[Int] = {
        (a1, a2) match {
            case (Nil, Nil) => Nil
            case (Nil, _) => a2
            case (_, Nil) => a1
            case _ if (a1.head <= a2.head) => a1.head :: merge(a1.tail, a2)
            case _  => a2.head :: merge(a1, a2.tail)
        }
    }      
}