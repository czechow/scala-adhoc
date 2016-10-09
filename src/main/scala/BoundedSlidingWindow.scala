
import BoundedSlidingWindow.{SizeExceeded, max}
import SlidingWindow.Check
import com.github.nscala_time.time.Imports._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec


//-------------------------------------------------------------------
//                        BoundedSlidingWindow
//-------------------------------------------------------------------

case class BoundedSlidingWindow private (maxSize: Int, sw: SlidingWindow, invalidTill: Option[DateTime]) {

  def add(dateTime: DateTime, value: Double): (BoundedSlidingWindow, Either[SizeExceeded, Check]) = {
    val (nsw, check) = sw.add(dateTime, value)

    if (nsw.heap.size <= maxSize)
      invalidTill match {
        case None => (this.copy(sw = nsw), Right(check))
        case Some(invTill) =>
          if (dateTime.isAfter(invTill)) (this.copy(sw = nsw, invalidTill = None), Right(check))
          else (this.copy(sw = nsw), Left(SizeExceeded(invTill)))
      }
    else {
      val (tnsw, delEl) = BoundedSlidingWindow.dropToSize(nsw, maxSize)(None)

      val nInvTill = (delEl, invalidTill) match {
        case (Some((dt, _)), Some(invTill)) => max(dt + nsw.timeWindow, invTill)
        case (Some((dt, _)), None) => dt + nsw.timeWindow
        case (None, Some(invTill)) => invTill
        case (None, None) => new DateTime("9999-12-31")
      }

      (this.copy(sw = tnsw, invalidTill = Some(nInvTill)), Left(SizeExceeded(nInvTill)))
    }
  }

  def size: Int = sw.size

  def reset: BoundedSlidingWindow = BoundedSlidingWindow(this.maxSize, sw.reset)
}


object BoundedSlidingWindow {
  def apply(maxSize: Int, limit: Double, timeWindow: Duration): BoundedSlidingWindow =
    BoundedSlidingWindow(maxSize, SlidingWindow(limit, timeWindow), None)
  def apply(maxSize: Int, sw: SlidingWindow): BoundedSlidingWindow = BoundedSlidingWindow(maxSize, sw)

  final case class SizeExceeded(invalidTill: DateTime)


  def max(dt1: DateTime, dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2

  @tailrec
  private def dropToSize(sw: SlidingWindow, size: Int)(mEl: Option[(DateTime, Double)]): (SlidingWindow, Option[(DateTime, Double)]) =
    if (sw.size <= size) (sw, mEl)
    else {
      val (nsw, mDelEl) = sw.delMin
      dropToSize(nsw, size)(mDelEl)
    }
}


//----------------------------------------------------------------------------------------
//                                      Properties
//----------------------------------------------------------------------------------------

object ScalaCheckBSWTest extends Properties("BoundedSlidingWindow") {

  def propSizeExceeded(bsw: BoundedSlidingWindow)(dateTime: DateTime, value: Double): Prop = {
    val (nbsw, eCheck) = bsw.add(dateTime, value)

    atLeastOne(
      "Size exceeded" |: (bsw.size == bsw.maxSize) ==> {
        eCheck match {
          case Left(SizeExceeded(_)) => true
          case _ => false
        }
      },
      "Size ok" |: bsw.size < bsw.maxSize ==> {
        eCheck match {
          case Right(_) => Prop(true)
          case _ => Prop.falsified
        }
      },
      "Size above max" |: bsw.size > bsw.maxSize ==> Prop.falsified
    )
  }

  def propInvTillMonoIncr(bsw: BoundedSlidingWindow)(dateTime: DateTime, value: Double): Prop = {
    val (nbsw, _) = bsw.add(dateTime, value)

    "InvTill monotonous increase" |: {
      (bsw.invalidTill, nbsw.invalidTill) match {
        case (None, None) => true
        case (None, Some(_)) => true
        case (Some(_), None) => true
        case (Some(invTill), Some(nInvTill)) => invTill <= nInvTill
      }
    }
  }


  def propMaxSizeZero(bsw: BoundedSlidingWindow)(dateTime: DateTime, value: Double): Prop = {
    val (nbsw, eCheck) = bsw.add(dateTime, value)

    atLeastOne(
      "Max size zero" |: bsw.maxSize <= 0 ==> {
        eCheck match {
          case Left(SizeExceeded(_)) => true
          case _ => false
        }
      },
      "Max size above zero" |: bsw.maxSize > 0 ==> Prop(true)
    )
  }


  val LIMIT = 100d
  val TIME_WIN = 10.seconds.toDuration

  property("Props") = forAll(genBSW(LIMIT, TIME_WIN)) { initBSW =>
    val eventsTimeWin = Duration.millis(3 * TIME_WIN.getMillis)
    forAll(genEvents(DateTime.now, eventsTimeWin)) { ls =>
      val (_, xs) = ls.foldLeft((initBSW, List[Prop]())) { case ((bsw, props), (dt, v)) =>
        val prop = all(
          propSizeExceeded(bsw)(dt, v),
          propInvTillMonoIncr(bsw)(dt, v),
          propMaxSizeZero(bsw)(dt, v)
        )

        val (nsw, _) = bsw.add(dt, v)
        (nsw, props :+ prop)
      }

      all(xs: _*)
    }
  }


  def genEvents(start: DateTime, timeSpan: Duration): Gen[List[(DateTime, Double)]] =
    Gen.nonEmptyListOf(
      Gen.zip(
        Gen.choose(0L, timeSpan.getMillis) map { ms => start + new Duration(ms) },
        Gen.choose(0L, 100L) map { l => l.toDouble }))

  def genBSW(limit: Double, timeWindow: Duration): Gen[BoundedSlidingWindow] = for {
    lt <- Gen.choose(0L, limit.toLong) map { l => l.toDouble }
    tw <- Gen.choose(0L, timeWindow.getMillis) map { ms => new Duration(ms) }
    sz <- Gen.choose(0, 3)
  } yield BoundedSlidingWindow(limit = lt, timeWindow = tw, maxSize = sz)


  def max(dt1: DateTime, dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2
}


