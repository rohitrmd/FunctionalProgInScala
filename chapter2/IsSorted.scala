object IsSorted {

def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(n: Int): Boolean =
    if (n >= as.length-1) true
    else if (gt(as(n), as(n+1))) false
    else go(n+1)

  go(0)
}

	def main(args:Array[String]):Unit = {
		val arr = Array(1,2,3,4)
		println(isSorted(arr, (x:Int,y:Int) => x>y))
	}
}

