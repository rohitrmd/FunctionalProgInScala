import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
	def map[B](f: A => B): Either[E, B] =
	{
		this match {
			case Right(a) => Right(f(a))
			case Left(a) => Left(f(a))
		}
	}

	

}
