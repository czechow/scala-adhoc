import scala.util.Random

import scalaz._
import Scalaz._

object MainScalaz {

  implicit class RightBiasedEither[A, B](e: Either[A, B]) {
    def map[C](f: B => C): Either[A, C] = e.right map f
    def flatMap[C](f: B => Either[A, C]) = e.right flatMap f
  }

  def main(args: Array[String]): Unit = {
    println("Up and running")

    f()
  }

  def f(): Unit = {
    val mi: List[Option[Int]] = List(1.some, 2.some, None)

    val sq: Option[List[Int]] = mi.sequence

    println(s"After sq: $sq")
  }
}
