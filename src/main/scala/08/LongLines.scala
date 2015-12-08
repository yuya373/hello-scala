import scala.io.Source

object LongLines {
  def processFile(filename: String, width: Int): Unit = {
    def processLine(line: String) =
      if (line.length > width)
        println(filename + ": " + line.trim)

    val source = Source.fromFile(filename)
    for (line <- source.getLines())
      processLine(line)
  }

  // private def processLine(filename: String, width: Int, line: String): Unit = {
  //   if (line.length > width)
  //     println(filename +": "+ line.trim)
  // }
}

object FindLingLines {
  def main(args: Array[String]) = {
    val width = args(0).toInt
    for (arg <- args.drop(1))
      LongLines.processFile(arg, width)
  }
}
