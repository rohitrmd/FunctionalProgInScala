trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  
    def toList[A]:List[A] = {
                @annotation.tailrec
                def go(stream:Stream[A], list:List[A]):List[A] = {
                        stream match {
                                case Cons(h, t) => go(t, h() :: list)
                                case _ => list
                        }
                }
                go(this, List[A]())
        }

        def take[A](n: Int): Stream[A] = {
                def go(stream:Stream[A], n:Int):Stream[A] = {
                        stream match {
                                case Cons(h, t) if n>1 => Cons(h(), go(t, n-1))
                                case Cons(h, _) if n == 1 => Cons(h(), empty)
                                case _ => empty

                        }
                }
                go(this, n)

        }

}

object Stream {
def empty[A]: Stream[A] =
  new Stream[A] { def uncons = None }
def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
  new Stream[A] {
    lazy val uncons = Some((hd, tl))
  }
def apply[A](as: A*): Stream[A] =
  if (as.isEmpty) empty
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


