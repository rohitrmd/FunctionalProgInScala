trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
    case class Simple(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
  	val (i, r) = rng.nextInt
	((if(i<0) -(i+1) else i), r)
  }

  def double(rng: RNG): (Double, RNG) = {
	val (i, r) = positiveInt(rng)
	((i/Int.MaxValue.toDouble+1), r)
  }  

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
	val (i, r1) = rng.nextInt
	val (d, r2) = double(r1)

	((i,d), r2)
  }  

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
	val (d, r1) = double(rng)
	val (i, r2) = r1.nextInt

	((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
	val (d1, r1) = double(rng)
	val (d2, r2) = double(r1)
	val (d3, r3) = double(r2)

	((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
	def go(n:Int, rng:RNG, acc:List[Int]):(List[Int], RNG) = {
		if(n == 0) {
			(acc, rng)
		} else {
			val (nxt, r) = rng.nextInt
			go(n-1, r,  nxt::acc)			
		}
	}
	go(count, rng, List[Int]())	
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def _double = {
  	 	
  }

}
