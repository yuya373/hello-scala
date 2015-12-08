object FileMatcher {
  private def filesHere = (new java.io.File(".")).listFiles
  // def filesEnding(query: String) =
  //   for {
  //     file <- filesHere
  //     if file.getName.endsWith(query)
  //   } yield file

  def filesMatching(matcher: String => Boolean) =
    for {
      file <- filesHere
      if matcher(file.getName)
    } yield file

  def filesEnding(q: String) =
    filesMatching(_.endsWith(q))

  def filesContaining(q: String) =
    filesMatching(_.contains(q))

  def filesRegex(q: String) =
    filesMatching(_.matches(q))
}
