package bobsrocketes {
  package navigation {
    private[bobsrocketes] class Navigator {
      val map = new StarMap
      private[navigation] def useStarChart: Unit = ()
      class LegOfJourney {
        private[Navigator] val distance = 100
      }
      private[this] var speed = 200
    }
    class StarMap

    package launch {
      class Booster1
    }

    class MissionControl {
      val booster1 = new launch.Booster1
      val booster2 = new bobsrocketes.launch.Booster2
      val booster3 = new _root_.launch.Booster3
    }
  }
  package launch {
    class Booster2
    import navigation._
    object Vehicle {
      private[launch] val guide = new Navigator
    }
  }
  class Ship {
    val nav = new navigation.Navigator
  }
  package fleets {
    class Fleet {
      def addShip = new Ship
    }
  }
}

package launch {
  class Booster3
}

class Super {
  protected def f = println("f")
}

class Sub extends Super {
  def subF = f
}

class Other {
  // def otherF = (new Super).f
}

class PPP {
  private var hoge = 1
  def incHoge(other: PPP): Unit = other.hoge += 1
  private[this] var fuga = 1
  // def incFuga(other: PPP): Unit = other.fuga + 1
  def incMyFuga: Unit = fuga += 1

  def show: Unit = println("hoge: " + hoge + ", fuga: " + fuga)
}

class Rocket {
  import Rocket.fuel
  private def canGoHomeAgain = fuel > 20
}
object Rocket {
  private def fuel = 10
  def chooseStrategy(rocket: Rocket) = {
    if (rocket.canGoHomeAgain)
      goHome
    else {
      pickAStar
    }
  }
  def goHome = println("Go home")
  def pickAStar = println("Pick a star")
}

package hoge {
}

package object hoge {
  val hoge = "hoge!!"
}
