object myList {
  val xs: List[String] = List()
  val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
  val nums = 1 :: 2 :: 3 :: 4 :: Nil
  val diag3 = (1 :: 0 :: 0 :: Nil) :: (0 :: 1 :: 0 :: Nil) :: (0 :: 0 :: 1 :: Nil) :: Nil
  val empty = Nil

  def insert(x: Int, xs: List[Int]): List[Int] =
    // if (xs.isEmpty || x <= xs.head) x :: xs
    // else xs.head :: insert(x, xs.tail)
    xs match {
      case List() => List(x)
      case y :: ys =>
        if (x <= y) x :: xs
        else y :: insert(x, ys)
    }

  def isort(xs: List[Int]): List[Int] =
    // if (xs.isEmpty) Nil
    // else insert(xs.head, isort(xs.tail))
    xs match {
      case List() => List()
      case x :: xs1 => insert(x, isort(xs1))
    }

  def append[T](xs: List[T], ys: List[T]): List[T] = {
    xs match {
      case List() => ys
      case h :: t => h :: append(t, ys)
    }
  }

  def length[A](l: List[A]): Int = {
    l match {
      case List() => 0
      case h :: t => 1 + length(t)
    }
  }
}

import myList._

val x = List(1,2,3)
val y = List(4,5,6)
val z = List(x, y)

x.zipWithIndex

val zipped = x zip y
zipped.unzip

println(z.mkString(" "))
println(z.map(_.mkString(" ")).mkString(" "))
