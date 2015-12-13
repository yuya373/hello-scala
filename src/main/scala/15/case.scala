package org.stairwaybook.expr
import org.stairwaybook.layout.Element
import Element.elem

sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object Expr {
  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => e
    case BinOp("+", e, Number(0)) => e
    case BinOp("*", e, Number(1)) => e
    case _ => expr
  }

  def generalSize(x: Any) = x match {
    case s: String => s.length
    case m: Map[_, _] => m.size
    case _ => -1
  }

  def isIntIntMap(x: Any) = x match {
// <console>:31: warning: non-variable type argument Int in type pattern scala.collection.immutable.Map[Int,Int] (the underlying of Map[Int,Int]) is unchecked since it is eliminated by erasure
//            case m: Map[Int, Int] => true
    case m: Map[Int, Int] => true
    case _ => false
  }

  def bindingMatch(x: Expr) = x match {
    case UnOp("abs", e @ UnOp("abs", _)) => e
    case _ => x
  }

  def simplifyAdd(x: Expr) = x match {
    // case BinOp("+", x, x) => BinOp("*", x, Number(2))
    case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
    case _ => x
  }

  def simplifyAll(x: Expr): Expr = x match {
    case UnOp("-", UnOp("-", e)) => simplifyAll(e)
    case BinOp("+", e, Number(0)) => simplifyAll(e)
    case BinOp("*", e, Number(1)) => simplifyAll(e)
    case UnOp(op, e) => UnOp(op, simplifyAll(e))
    case BinOp(op, l, r) => BinOp(op, simplifyAll(l), simplifyAll(r))
    case _ => x
  }

  def describe(e: Expr): String = (e: @unchecked) match {
    case Number(_) => "a number"
    case Var(_) => "a variable"
  }
}

// import Expr._

// // val expr = UnOp("abs", UnOp("abs", UnOp("-", Number(2))))
// // bindingMatch(expr)

// // val add = BinOp("+", Number(3), Number(3))
// // simplifyAdd(add)

// // val all = BinOp(
// //   "*",
// //   UnOp("-", UnOp("-", Number(3))),
// //   BinOp(
// //     "+",
// //     BinOp("*", Number(3), Number(1)),
// //     BinOp("+", Number(1), Number(0))
// //   )
// // )
// // simplifyAll(all)

// val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
// val exp = new BinOp("*", Number(5), Number(1))

// val withDefault: Option[Int] => Int = {
//   case Some(x) => x
//   case None => 0
// }

// val second: List[Int] => Int = {
//   case x::y::_ => y
// }
// // <console>:25: warning: match may not be exhaustive.
// // It would fail on the following inputs: List(_), Nil
// //        val second: List[Int] => Int = {
// //                                       ^
// // second: List[Int] => Int = <function1>

// val pSecond: PartialFunction[List[Int], Int] = {
//   case x::y::_ => y
// }
// // pSecond: PartialFunction[List[Int],Int] = <function1>

// pSecond.isDefinedAt(List(5,6,7))
// pSecond.isDefinedAt(List())

// second.isDefinedAt(List(5,6,7))
// // <console>:27: error: value isDefinedAt is not a member of List[Int] => Int
// //        second.isDefinedAt(List(5,6,7))

// new PartialFunction[List[Int], Int] {
//   def apply(xs: List[Int]) = xs match {
//     case x::y::_ => y
//   }

//   def isDefinedAt(xs: List[Int]) = xs match {
//     case x::y::_ => true
//     case _ => false
//   }
// }

// for {
//   (country, city) <- capitals
// } println("The capital of " + country + " is " + city)

// val results = List(Some("apple"), None, Some("orange"))

// for (Some(fruit) <- results) println(fruit)


class ExprFormatter {
  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )
  private val precedence = {
    val assocs = for {
      i <- 0 until opGroups.length
      op <- opGroups(i)
    } yield op -> i
    assocs.toMap
  }
  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e: Expr, enclPrec: Int): Element =
    e match {
      case Var(name) => elem(name)
      case Number(num) =>
        def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s
        elem(stripDot(num.toString))
      case UnOp(op, arg) =>
        elem(op) beside format(arg, unaryPrecedence)
      case BinOp("/", left, right) =>
        val top = format(left, fractionPrecedence)
        val bot = format(right, fractionPrecedence)
        val line = elem('-', top.width max bot.width, 1)
        val frac = top above line above bot
        if (enclPrec != fractionPrecedence) frac
        else elem(" ") beside frac beside elem(" ")
      case BinOp(op, left, right) =>
        val opPrec = precedence(op)
        val l = format(left, opPrec)
        val r = format(right, opPrec)
        val oper = l beside elem(" " + op + " ") beside r
        if (enclPrec <= opPrec) oper
        else elem("(") beside oper beside elem(")")
    }
  def format(e: Expr): Element = format(e, 0)
}
