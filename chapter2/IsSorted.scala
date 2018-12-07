def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(idx: Int): Boolean = {
    if (idx == as.length) true
    else if (!ordered(as(idx-1), as(idx))) false
    else loop(idx + 1)
  }

  loop(1)
}    

def ascending(x: Int, y: Int) = {
    y > x
}

println(isSorted(Array(1, 4, 5), ascending))
println(isSorted(Array(9, 1, 2, 3, 4), ascending))
println(isSorted(Array(5, 4, 3, 2, 1), (x: Int, y: Int) => y < x))
