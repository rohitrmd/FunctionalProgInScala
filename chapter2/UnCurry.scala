object UnCurry {

	def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
		(a, b) => f(a)(b)
	}

	def main(args:Array[String]):Unit = {
		val uc= uncurry[Int, Int, Int]((a:Int) => (b:Int) => a+b)

		println(uc(1, 2))
	}
}
