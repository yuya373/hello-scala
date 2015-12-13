object myList {
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

  def msort[T](l: List[T])(isLessThan: (T, T) => Boolean): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (isLessThan(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = l.length / 2
    if (n == 0) l
    else {
      val (a, b) = l splitAt n
      merge(msort(a)(isLessThan), msort(b)(isLessThan))
    }
  }

  def hasUpperCase(s: String) = s.exists(_.isUpper)

  def countWords(text: String): Map[String, Int] = {
    val counts = scala.collection.mutable.Map.empty[String, Int]
    for (rawWord <- text.split("[ ,!.]+")) {
      val word = rawWord.toLowerCase
      val oldCount = if (counts.contains(word)) counts(word)
                     else 0
      counts += (word -> (oldCount + 1))
    }
    counts.toMap
  }

  def longestWord(words: Array[String]) = {
    var word = words(0)
    var idx = 0
    for (i <- 1 until words.length)
      if (words(i).length > word.length) {
        word = words(i)
        idx = i
      }
    (word, idx)
  }
}

// import myList._

// val x = List(1,2,3)
// val y = List(4,5,6)
// val z = List(x, y)

// val xy = x ::: y

// // val xs: List[String] = List()
// val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
// val nums = 1 :: 2 :: 3 :: 4 :: Nil
// val diag3 = (1 :: 0 :: 0 :: Nil) :: (0 :: 1 :: 0 :: Nil) :: (0 :: 0 :: 1 :: Nil) :: Nil
// val empty = Nil

// val words = List("the", "quick", "brown", "fox")

// import scala.collection.mutable.ListBuffer

// val buf = new ListBuffer[Int]

// buf += 1
// buf += 2
// 3 +=: buf
// val bufList = buf.toList
// buf += 4

// val bufList2 = buf.toList

// import scala.collection.mutable.ArrayBuffer

// val aBuf = new ArrayBuffer[Int]()

// aBuf += 12
// aBuf += 15
// aBuf.length
// aBuf(0)

// import scala.collection.mutable
// val mutaSet = mutable.Set(1, 2, 3)

// val text = "See Spot run. Run, Spot. Run!"
// val wordsArray = text.split("[ !,.]+")
// val words = mutable.Set.empty[String]

// for (word <- wordsArray)
//   words += word.toLowerCase

// val nums = Set(1, 2, 3)
// nums + 5
// nums - 3
// nums ++ List(5, 6)
// nums -- List(1, 2)
// nums & Set(1, 3, 5, 7)
// nums.size
// nums.contains(3)

// import scala.collection.mutable
// val words = mutable.Set.empty[String]
// words += "the"
// words -= "the"
// words ++= List("do", "re", "mi")
// words --= List("do", "re")
// words.clear
// words

// val map = mutable.Map.empty[String, Int]
// map("hello") = 1
// map("there") = 2
// map("hello")

// countWords("See Spot run! Run, Spot. Run!")

// val nums = Map("i" -> 1, "ii" -> 2)
// nums + ("vi" -> 6)
// nums - "ii"
// val tapList = List("iii" -> 3, "v" -> 5)
// nums ++ tapList
// nums -- List("i", "ii")
// nums.size
// nums("ii")
// nums.keys
// nums.keySet
// nums.values
// nums.isEmpty

// import scala.collection.mutable
// val words = mutable.Map.empty[String, Int]
// words += ("one" -> 1)
// words -= ("one")
// words
// words ++= List("one" -> 1, "two" -> 2, "three" -> 3)
// words --= List("one", "two")

// import scala.collection.immutable.TreeSet
// val ts = TreeSet(9, 3, 1, 8, 0, 0, 2, 7, 4, 6, 5)
// val cs = TreeSet('f', 'u', 'n')

// var vm = scala.collection.immutable.TreeMap(3 -> 'x', 1 -> 'x', 4 -> 'x')
// vm += (2 -> 'x')
// vm

// val people = Set("Nancy", "Jane")
// people += "Bob"

// var people = Set("Nancy", "Jane")
// people += "Bob"
// people -= "Jane"
// people ++= List("Tom", "Harry")
// people

// val stuff = scala.collection.mutable.Set[Any](42)
// stuff += "abracadabra"

// val colors = List("blue", "yellow", "red", "green")
// val ts = scala.collection.immutable.TreeSet(colors)

// val ts = scala.collection.immutable.TreeSet[String]() ++ colors

// val longest = longestWord("The quick brown fox".split(" "))
// longest._1
// longest._2
// val (word, idx) = longest

