object Compose {
        def compose[A,B,C](f: B => C, g: A => B): A => C = { a => f(g(a))}  

	def main(args:Array[String]):Unit = {
		val double = (a:Int) => a*2
		val toString = (a:Int) => "New value = "+ a

		val composed = compose(toString, double)

		println(composed(2))

	}

}
