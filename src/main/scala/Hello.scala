object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }

  def makeRowSeq(row: Int): IndexedSeq[String] =
    for (col <- 1 to 10) yield {
      val prod = (row * col).toString
      val padding = " " * (4 - prod.length)
      padding + prod
    }

  def makeRow(row: Int): String = makeRowSeq(row).mkString

  def multiTable() = {
    val tableSeq = for (row <- 1 to 10) yield makeRow(row)
    tableSeq.mkString("\n")
  }

  def boom(x: Int): Int =
    if (x == 0) throw new Exception("boom!")
    else boom(x - 1) + 1

  def bang(x: Int): Int =
    if (x == 0) throw new Exception("bang!")
    else bang(x - 1)

  def isEven(x: Int): Boolean =
    if (x == 0) true else isOdd(x - 1)
  def isOdd(x: Int): Boolean =
    if (x == 0) false else isEven(x - 1)
}

