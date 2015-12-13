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

trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def enqueue(x: T): Queue[T]
}

object Queue {
  // def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[T] (
    private val leading: List[T],
    private val trailing: List[T]
  ) extends Queue[T] {
    def mirror =
      if (leading.isEmpty)
        new QueueImpl(trailing.reverse, Nil)
      else
        this
    def head: T = mirror.leading.head
    def tail: QueueImpl[T] = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }
    def enqueue(x: T) =
      new QueueImpl(leading, x :: trailing)
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

