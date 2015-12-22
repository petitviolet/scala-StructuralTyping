package net.petitviolet.sandbox

//import org.openjdk.jmh.annotations.Benchmark


import scala.language.reflectiveCalls

object Param {
  sealed abstract class AwesomeData(id: Int)

  case class Nice(id: Int) extends AwesomeData(id) {
    def size: Int = id * 100
    def calc(n: Int): Int = n * id
  }

  case class Great(id: Int) extends AwesomeData(id) {
    def size: Int = id * 200
    def calc(n: Int): Int = 2 * n * id
  }
  val nice = Nice(1)
  val great = Great(2)
  val seq = Seq(1, 2, 3)

  val num = 10000
  val x = 999
}

class DuckTypingJustCall {
  import Param._

  def callSizeReflection[A](a: A) = {
    val method = a.getClass.getMethod("size")
    val size = method.invoke(a)

    size.asInstanceOf[Int]
  }

  type HasSize = { def size: Int }
  def callSizeDuck[A <: HasSize](hasSize: A) = {
    hasSize.size
  }
//
//  @Benchmark
//  def benchSizeReflection(): Unit = {
//    (0 to num) foreach (_ => callSizeReflection(nice))
//  }
//
//  @Benchmark
//  def benchSizeDuck(): Unit = {
//    (0 to num) foreach (_ => callSizeDuck(nice))
//  }
}

class DuckTypingWithArg {
  import Param._

  def callCalcReflection[A](a: A) = {
    val method = a.getClass.getMethod("calc", classOf[Int])
    val calc = method.invoke(a, x.asInstanceOf[AnyRef])

    calc.asInstanceOf[Int]
  }

  type HasCalc = { def calc(n: Int): Int }
  def callCalcDuck[A <: HasCalc](hasCalc: A) = {
    hasCalc.calc(x)
  }

//  @Benchmark
//  def benchCalcReflection(): Unit = {
//    (0 to num) foreach (_ => callCalcReflection(nice))
//  }
//
//  @Benchmark
//  def benchCalcDuck(): Unit = {
//    (0 to num) foreach (_ => callCalcDuck(nice))
//  }
}


