//import Monoid.IntMonoid
//import Monad.OptionMonad
import BoundedDailyLimit.{CheckResult, DateRangeExceeded, SecLimitExceeded}
import CacheLimit.{Check, LimitExceeded, WithinLimit}
import Monad._
import org.joda.time.DateTime

import scala.collection.SortedMap

//import scalaz.Heap
//import scalaz.Heap.Empty
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
/*
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
*/

    val x = read(DateTime.now, 1024)(kdbReadFn("MyProps"))

    println("x: " + x)
  }

  def getStream = Stream.range[Long](1, 1e8.toLong)

  type Result = (String, Int)
  type ErrMsg = String

  def read(dateTime: DateTime, maxSize: Int)
          (rdFn: (DateTime) => List[String]): Either[ErrMsg, List[Result]] = {
    val ls = rdFn(dateTime)
    if (ls.size > maxSize) Left("Failed => size too large")
    else {
      Right(ls map { x => (x, x.length) })
    }
  }

  def kdbReadFn(kdbProps: String): DateTime => List[String] = {
    val conn = 34

    x: DateTime => {
      val z = conn.equals(342)
      // read all stuff...
      List("Tom", "Jerry")
    }
  }

}


final case class Date private (date: DateTime)

object CacheLimit {
  sealed trait Check
  final case class WithinLimit() extends Check
  final case class LimitExceeded(v: Double) extends Check

  def apply(limit: Double): CacheLimit = CacheLimit(limit, Map.empty)
}

final case class CacheLimit private (limit: Double, data: Map[String, Double]) {
  def add(secId: String, qty: Double): (CacheLimit, Check) = {
    val currVal = data.getOrElse(secId, 0d)
    val nVal = currVal + qty
    val check = if (nVal <= limit) WithinLimit() else LimitExceeded(nVal)

    (this.copy(data = data.updated(secId, nVal)), check)
  }

  def size = data.size
}

object BoundedCacheLimit {
  def apply(maxSize: Int, limit: Double): BoundedCacheLimit =
    BoundedCacheLimit(maxSize, CacheLimit(limit))

}

final case class BoundedCacheLimit private (maxSize: Int, cl: CacheLimit) {

  def add(secId: String, qty: Double): (BoundedCacheLimit, Either[String, Check]) = {
    if (cl.size >= maxSize) (this, Left("Max security count reached"))
    else {
      val (nCl, check) = cl.add(secId, qty)
      (this.copy(cl = nCl), Right(check))
    }
  }
}

object BoundedDailyLimit {
  sealed trait Result
  final case class CheckResult(check: CacheLimit.Check) extends Result
  final case class SecLimitExceeded(l: Int) extends Result
  final case class DateRangeExceeded(dMin: Date, dMax: Date) extends Result

  def apply(maxSize: Int, limit: Double) =
    BoundedCacheLimit(maxSize, CacheLimit(limit))
}

final case class BoundedDailyLimit(limit: Double, maxDays: Int, data: SortedMap[Date, BoundedCacheLimit]) {
  def add(date: Date, secId: String, qty: Double)
  : (BoundedDailyLimit.Result, BoundedDailyLimit) = {

    val bcl = data.getOrElse(date, BoundedCacheLimit(128, limit))
    val (nBcl, dec) = bcl.add(secId, qty)

    val maxDate = data.lastOption match {
      case Some((md, _)) => if (md.date.isAfter(date.date)) md else date
      case None => date
    }

    val minDate = Date(maxDate.date.minusDays(maxDays)) // FIXME: DST safe??/
    val nData = data.updated(date, nBcl) filter { case (dt, _) => ! dt.date.isBefore(minDate.date) } // Should be short

    (nData.get(date), dec) match {
      case (Some(_), Right(check)) => (CheckResult(check), this.copy(data = nData))
      case (Some(_), Left(errMsg)) => (SecLimitExceeded(1313), this.copy(data = nData)) // FIXME
      case (None, _) => (DateRangeExceeded(minDate, maxDate), this)
    }
  }
}
