object Option {
	def mean(l:List[Double]):Option[Double] = {
		l match {
			case Nil => None
			case _ => Some(l.sum/l.length)
		}
	}

	def variance(l:List[Double]):Option[Double] = {
		mean(l) flatMap (m => mean(l map (x => math.pow(x-m, 2))))
	}

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		a flatMap(aa => b map(bb => f(aa, bb)))
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		a match {
			case Nil => Some(Nil)
			case x::xs => x flatMap(xx => sequence(xs) map(
		}
	}

	def main(args:Array[String]) {
		val list = List(1.0, 2, 3, 4)
		println("Mean = "+Option.mean(list))
		println("Variance = "+Option.variance(list))
		val a = Some(2)
		val b = Some(3)
		println("Map2 = "+Option.map2(a, b)((a,b) => a+b))
	}

}

