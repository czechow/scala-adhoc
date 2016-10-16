//import Monoid.IntMonoid
//import Monad.OptionMonad
import Monad._
//import scalaz.Foldable

//import scalaz._
//import Scalaz._

trait Monoid[A] {
  def mappend(i1: A, i2: A): A
  def mzero: A
}

object Monoid {
  implicit object IntMonoid extends Monoid[Int] {
    def mappend(i1: Int, i2: Int): Int = i1 + i2
    def mzero: Int = 0
  }

  implicit object StringMonoid extends Monoid[String] {
    def mappend(i1: String, i2: String): String = i1 + i2
    def mzero: String = ""
  }
}

trait Foldable[F[_]] {
  def foldLeft[A, B](xs: F[A])(z: B)(f: (B, A) => B): B
}

object Foldable {
  implicit object ListFoldable extends Foldable[List] {
    def foldLeft[A, B](xs: List[A])(z: B)(f: (B, A) => B): B = xs.foldLeft(z)(f)
  }
}

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def bind[A, B](ma: F[A])(f: A => F[B]): F[B]
  def fmap[A, B](ma: F[A])(f: A => B): F[B] = bind(ma) { a => unit(f(a)) }
  def >>=[A, B](ma: F[A])(f: A => F[B]): F[B] = bind(ma)(f)
}

object Monad {
  implicit object OptionMonad extends Monad[Option] {
    def unit[A](a: A): Option[A] = Some(a)
    def bind[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma match {
      case Some(a) => f(a)
      case None => None
    }
  }

  implicit class MonadOps[F[_], A](ma: F[A]) {
    def >>=[B](f: A => F[B])(implicit m: Monad[F]): F[B] = m.>>=(ma)(f)
    def fmap[B](f: A => B)(implicit m: Monad[F]): F[B] = m.fmap(ma)(f)
    def `<$>`[B](f: A => B)(implicit m: Monad[F]): F[B] = m.fmap(ma)(f)
  }
}


object AdhocSW {
  import Monad.OptionMonad._

  implicit class RightBiasedEither[A, B](e: Either[A, B]) {
    def map[C](f: B => C): Either[A, C] = e.right.map(f)
    def flatMap[C](f: B => Either[A, C]): Either[A, C] = e.right.flatMap(f)
  }

  def sum[A](l: List[A])(implicit m: Monoid[A]): A =
    l.foldLeft(m.mzero) { (acc, x) => m.mappend(acc, x) }

  def sum2[M[_], A](xs: M[A])(implicit m: Monoid[A], fl: Foldable[M]): A = {
    fl.foldLeft(xs)(m.mzero)(m.mappend)
  }

//    xs.foldLeft(m.mzero) { (acc, x) => m.mappend(acc, x) }


  def main(args: Array[String]): Unit = {

    val i = sum2(List(1, 2, 3 ,4))
    println("Sum is " + i)


    val s = sum2(List("Tom", "Jerry", "Don"))
    println("Sum is " + s)

    val x = unit("TEST")
    val y = unit(13)

    val z = bind(x) { x => Some(x + "-" + x)}

    val zzz = z >>= { x => Some(x) }
    val z2 = z fmap { x => x }
    val z3 = z `<$>` { x => x }

    println(s"$x, $y, $z, $zzz, $z2, $z3")

  }
}

