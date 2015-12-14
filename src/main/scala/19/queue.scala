class SlowAppendQueue[T](elems: List[T]) {
  def head = elems.head
  def tail = new SlowAppendQueue(elems.tail)
  def enqueue(x: T) = new SlowAppendQueue(elems ::: List(x))
}

class SlowHeadQueue[T](elems: List[T]) {
  def head = elems.last
  def tail = new SlowHeadQueue(elems.init)
  def enqueue(x: T) = new SlowHeadQueue(x :: elems)
}

// class Queue[T] private (
//   private val leading: List[T],
//   private val trailing: List[T]
// ) {
//   private def mirror =
//     if (leading.isEmpty)
//       new Queue(trailing.reverse, Nil)
//     else
//       this

//   def head = mirror.leading.head
//   def tail = {
//     val q = mirror
//     new Queue(q.leading.tail, q.trailing)
//   }
//   def enqueue(x: T) = new Queue(leading, x :: trailing)
// }

trait Queue[+T] {
  def head: T
  def tail: Queue[T]
  def enqueue[A >: T](x: A): Queue[A]
  def inspect: Unit
}

object Queue {
  // def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
  // def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

  // private class QueueImpl[T] (
  //   private val leading: List[T],
  //   private val trailing: List[T]
  // ) extends Queue[T] {
  //   def mirror =
  //     if (leading.isEmpty)
  //       new QueueImpl(trailing.reverse, Nil)
  //     else
  //       this
  //   def head: T = mirror.leading.head
  //   def tail: QueueImpl[T] = {
  //     val q = mirror
  //     new QueueImpl(q.leading.tail, q.trailing)
  //   }
  //   def enqueue[A >: T](x: A) =
  //     new QueueImpl[A](leading, x :: trailing)

  //   def inspect = {
  //     println("leading: " + leading)
  //     println("trailing: " + trailing)
  //   }
  // }

  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[+T] (
    private[this] var leading: List[T],
    private[this] var trailing: List[T]
  ) extends Queue[T] {
    private def mirror() =
      if (leading.isEmpty) {
        while (!trailing.isEmpty) {
          leading = trailing.head :: leading
          trailing = trailing.tail
        }
      }
    def head: T = {
      mirror()
      leading.head
    }
    def tail: Queue[T] = {
      mirror()
      new QueueImpl[T](leading.tail, trailing)
    }
    def enqueue[A >: T](x: A) =
      new QueueImpl[A](leading, x :: trailing)
    def inspect: Unit = {
      println("leading: " + leading)
      println("trailing: " + trailing)
    }
  }
}

class Cell[T](init: T) {
  private[this] var current = init
  def get = current
  def set(x: T) = current = x
}

// trait IntQueue extends Queue[Int] {
//   abstract override def enqueue(x: Int) = {
//     println(math.sqrt(x))
//     super.enqueue(x)
//   }
// }
// class StrangeIntQueue extends IntQueue {
// }

trait OutputChannel[-T] {
  def write(x: T) = ???
}

class Person(val firstName: String, val lastName: String)
    extends Ordered[Person] {
  def compare(that: Person) = {
    val lastNameComparison = lastName.compareToIgnoreCase(that.lastName)
    if (lastNameComparison != 0)
      lastNameComparison
    else
      firstName.compareToIgnoreCase(that.firstName)
  }
  override def toString = firstName + " " + lastName
}

object UpperBoundExample {
  val robert = new Person("Robert", "Jones")
  val sally = new Person("Sally", "Smith")
  val people = List(
    new Person("Larry", "Wall"),
    new Person("Anders", "hejlsberg"),
    new Person("Guido", "van Rossum"),
    new Person("Alan", "Kay"),
    new Person("Yukihiro", "Matsumoto")
  )

  // 継承ツリー上の自分より上にOrdered[T]があるやつ
  def orderedMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt(n)
      merge(orderedMergeSort(ys), orderedMergeSort(zs))
    }
  }

  def run = println(orderedMergeSort(people))
}

