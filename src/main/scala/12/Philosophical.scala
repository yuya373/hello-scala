trait Philosophical {
  def philosophize :Unit =
    println("I consume memory, therefore I am!")
}

class Animal
class Frog extends Animal with Philosophical {
  override def toString = "green"
  override def philosophize =
    println("It ain't easy being " + toString + "!")
}
