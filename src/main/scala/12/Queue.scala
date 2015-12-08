abstract class IntQueue {
  def get: Int
  def put(x: Int): Unit
}

class BasicIntQueue extends IntQueue {
  import scala.collection.mutable.ArrayBuffer

  private val buf = new ArrayBuffer[Int]

  def get: Int = buf.remove(0)
  def put(x: Int): Unit = buf += x
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int): Unit = super.put(2 * x)
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int): Unit = super.put(x + 1)
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int): Unit = if (x >= 0) super.put(x)
}
