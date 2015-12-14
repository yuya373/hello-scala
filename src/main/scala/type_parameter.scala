trait Monoid[A] {
  def mappend(a1: A, a2: A): A
  def mzero: A
}

// object IntMonoid extends Monoid[Int] {
//   def mappend(a: Int, b: Int): Int = a + b
//   def mzero: Int = 0
// }

object Monoid {
  implicit object IntMonoid extends Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }

  implicit object StringMonoid extends Monoid[String] {
    def mappend(a: String, b: String): String = a + b
    def mzero: String = ""
  }
}

object FoldLeftList {
  def foldLeft[A, B](xs: List[A])(b: B)(f: (B, A) => B) = xs.foldLeft(b)(f)
}

// import scala.language.higherKinds

trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A])(b: B)(f: (B, A) => B): B
}

object FoldLeft {
  implicit object FoldLeftList extends FoldLeft[List] {
    def foldLeft[A, B](xs: List[A])(b: B)(f: (B, A) => B): B = xs.foldLeft(b)(f)
  }
}

object Main {
  // implicit val intMonoid = IntMonoid
  // ↑ なぜ消しても動く?
  // import Monoid._

  val multiMonoid = new Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a * b
    def mzero: Int = 1
  }

  // def sum[T](xs: List[T])(implicit m: Monoid[T]): T =
  //   FoldLeftList.foldLeft(xs)(m.mzero)(m.mappend)

  def sum[M[_], T](xs: M[T])(implicit m: Monoid[T], fl: FoldLeft[M]): T =
    fl.foldLeft(xs)(m.mzero)(m.mappend)

  def p(a: Any) = println("###> " + a)
  def run = {
    println

    p(sum(List(1, 2, 3, 4)))
    p(sum(List("a", "b", "c", "d")))
    p(sum(List(1, 2, 3, 4))(multiMonoid, implicitly[FoldLeft[List]]))

    println
  }
}
