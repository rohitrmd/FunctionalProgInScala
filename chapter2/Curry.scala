object Curry {
	def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
		a => b => f(a,b)

	}

	def main(args:Array[String]):Unit = {
		val c = curry((a:Int, b:Int) => a == b)
		 println("1 == 2? ", c(1)(2))
	}
}
