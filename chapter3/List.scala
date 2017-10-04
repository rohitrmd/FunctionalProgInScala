package fpinscala.datastructures
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
}
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
}
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1,2,3)
  val total = sum(example)

  def tail[A](as: List[A]):List[A] = {
	as match {
		case Nil => Nil
		case Cons(head, tail) => tail 
	}
 }

 def drop[A](l: List[A], n: Int): List[A] = {
	l match {
		case Nil => Nil
		case _ if n ==0 => l
		case Cons(head, tail) => drop(tail, n-1)
	}
 }


 def setHead[A](l: List[A], h: A): List[A] = l match {
  case Nil => sys.error("setHead on empty list")
  case Cons(_,t) => Cons(h,t)
}
 
 def init[A](l:List[A]):List[A] = {
 	l match {
		case Nil => sys.error("error with empty list")
		case Cons(_, Nil) => Nil
		case Cons(h, t) => Cons(h, init(t))
	}
 }

 def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
  l match {
	 case Nil => z
   	 case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc+1)
  }
  
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
  	l match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x)) (f)
	}
  }

  def lengthLeft[A](l:List[A]):Int = {
	foldLeft(l, 0)((acc, _) => acc+1)
  }

  def sum2(l:List[Int]) = foldLeft(l, 0)(_ + _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def addOne(l:List[Int]):List[Int] = {
	l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(x+1, addOne(xs))
	}
  }

  def convertToString(l:List[Double]):List[String] = {
	l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(x.toString, convertToString(xs))
	}
  }
  
  def map[A,B](l: List[A])(f: A => B): List[B] = {
	l match {
		case Nil => Nil
		case Cons(x, xs) => Cons(f(x), map(xs)(f))
	}
  }

  def filter[A](l:List[A])(f:A => Boolean):List[A] = {
	l match {
		case Nil => Nil
		case Cons(x, xs) => if(f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
	}
  }

  def append[A](l1:List[A], l2:List[A]):List[A] = {
	l1 match {
		case Nil => l2
		case Cons(x, xs) => Cons(x, append(xs, l2))
	}
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
	foldRight(l, List[B]())((a, b) => append(f(a), b))
  }
}

object Solution {
	 def main(args:Array[String]):Unit = {
        import List._
	val list = List(1, 2, 3)
	val list2 = List(4, 5)
	val dList:List[Double] = List(1, 2, 4)
        println("After calling tail = "+List.tail(list))
	println("After calling drop = "+List.drop(list, 2))
	println("After setting head to 4 =" + List.setHead(list, 4))
        println("After init =" + List.init(list)) 
	println("Length of list =" + List.length(list))
	println("Length in fold left = "+List.lengthLeft(list))
	println("Sum in fold left = "+List.sum2(list))
	println("Reversed = "+List.reverse(list))
	println("After adding one = "+ List.addOne(list))
	println("Using map function =" + List.map(list)({_*3}))
	println("After converting elements to strings =" + List.convertToString(dList))
	println("Adding filter to select even numbers =" + List.filter(list)(x => x%2 == 0))
	println("After appending lists =" +List.append(list, list2))
	println("After flatmap =" +List.flatMap(list)(i => List(i, i)))
}

}
