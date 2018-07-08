object Sort {
  def mergeSort(input: List[Int]): List[Int] = {
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

  def merge(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => a2
      case (_, Nil) => a1
      case _ if a1.head <= a2.head => a1.head :: merge(a1.tail, a2)
      case _ => a2.head :: merge(a1, a2.tail)
    }
  }    
}