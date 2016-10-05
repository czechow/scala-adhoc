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

    val e: Either[String, Int] = Right(10)

    def buildValue(pt: String): Either[String, Int] = Right(Random.nextInt(150) - 50)

    def sane(pt: String)(v: Int): Either[String, Int] =
      if (v <= 50) Right(v)
      else         Left(s"$pt above 50: $v")

    def nonNeg(pt: String)(v: Int): Either[String, Int] =
      if (v >= 0) Right(v)
      else        Left(s"$pt negative value: $v")


    def validate(pt: String)(validators: (String => Int => Either[String, Int]) *): Either[String, Int] =
      (validators map { f => f(pt) }).foldLeft(buildValue(pt)){ _ flatMap _ }

    (for {
      t <- validate("x")(sane, nonNeg, pt => v => Right(v))
      s <- validate("y")(sane, nonNeg)
    } yield {
      (t, s)
    }) match {
      case v => println(s"V: [$v]")
//      case Left(e1) => println(s"L: $e1")
//      case Right(m) => println(s"R: $m")
    }

    f()
  }

  def f(): Unit = {
    val mi: List[Option[Int]] = List(1.some, 2.some, None)

    val sq: Option[List[Int]] = mi.sequence

    println(s"After sq: $sq")
  }
}
