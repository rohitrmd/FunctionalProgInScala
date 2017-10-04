object Fibonacci {
	def fib(n: Int): Int = {
		
		def go(n:Int, prev:Int, acc:Int):Int = {
			if(n<=1){
				return acc
			}
			else { 
			   go(n-1, acc, prev+acc)
			} 
		}
		go(n, 1, 1)
	}

	def main(args:Array[String]):Unit = {
		println(fib(5))
	}
}
