


object sdfdsf {
  val ml = (x: Int, y: Int) => (x * y, x / y)

  def update(data: Int): Int => (Int, (Int, Int, Int)) =
    { ml.curried(data)(_)          } andThen { case ((os, chk1)) =>
      (ml(data, os), chk1)         } andThen { case ((os, chk2), chk1) =>
      (ml(data, os), (chk1, chk2)) } andThen { case ((os, chk3), (chk1, chk2)) =>
      (os, (chk1, chk2, chk3))
    }

}


/*
import com.github.nscala_time.time.Imports._
import org.scalacheck.Prop.{AnyOperators, _}
import org.scalacheck._

import scala.annotation.tailrec


sealed trait Check
final case class WithinLimit() extends Check
final case class LimitExceeded(currVal: Double) extends Check

//-------------------------------------------------------------------
//                               SW
//-------------------------------------------------------------------

case class SW(limit: Double,
              timeWindow: Duration,
              heap: List[(DateTime, Double)] = List()) {

  def add(dateTime: DateTime, value: Double): (SlidingWindow, Check) = {
    val boundary = latestEventTime match {
      case None     => dateTime - timeWindow
      case Some(le) => (if (le > dateTime) le else dateTime) - timeWindow
    }

    val newHeap = ((dateTime, value) :: heap).filter { case (dt, _) => ! dt.isBefore(boundary) }.sorted

    val currVal = SlidingWindow.heapSum(newHeap)
    val check = if (currVal <= limit) WithinLimit() else LimitExceeded(currVal)

    (this.copy(heap = newHeap), check)
  }

  def sum: Double = SlidingWindow.heapSum(heap)

  def latestEventTime: Option[DateTime] = heap match {
    case Nil => None
    case xs => Some(xs.max._1)
  }

  def delMin: (SlidingWindow, (Option[(DateTime, Double)])) = heap match {
    case Nil => (this, None)
    case (x :: xs) => (this.copy(heap = xs), Some(x))
  }

  // FIXME: add tests here
  def reset: SlidingWindow = SlidingWindow(this.limit, this.timeWindow)

  // FIXME: not tested...

  def size: Int = heap.size
}

object SW {
  def apply(limit: Double, timeWindow: Duration) = new SlidingWindow(limit, timeWindow)

  def heapSum(heap: List[(DateTime, Double)]): Double =
    heap.foldLeft(0d) { case (acc, (_, v)) => acc + v }
}

//-------------------------------------------------------------------
//                               BSW
//-------------------------------------------------------------------


sealed trait SizeFailure
final case class SizeExceeded(invalidTill: DateTime) extends SizeFailure
final case class SizeExceededRecovery(invalidTill: DateTime) extends SizeFailure



case class BSW private (maxSize: Int, sw: SlidingWindow, invalidTill: Option[DateTime]) {

  def add(dateTime: DateTime, value: Double): (BSW, Either[SizeFailure, Check]) = {
    val (nsw, check) = sw.add(dateTime, value)

    if (nsw.heap.size <= maxSize)
      invalidTill match {
        case None => (this.copy(sw = nsw), Right(check))
        case Some(invTill) =>
          if (dateTime.isAfter(invTill)) (this.copy(sw = nsw, invalidTill = None), Right(check))
          else (this.copy(sw = nsw), Left(SizeExceededRecovery(invTill)))
      }
    else {
      val (tnsw, delEl) = BSW.dropToSize(nsw, maxSize)(None)

      val nInvTill = (delEl, invalidTill) match {
        case (Some((dt, _)), Some(invTill)) => BSW.max(dt + nsw.timeWindow, invTill)
        case (Some((dt, _)), None) => dt + nsw.timeWindow
        case (None, Some(invTill)) => invTill
        case (None, None) => new DateTime("9999-12-31") // Will tihs throw???
      }

      (this.copy(sw = tnsw, invalidTill = Some(nInvTill)), Left(SizeExceeded(nInvTill)))
    }
  }

  def size: Int = sw.size

  def reset: BSW = BSW(this.maxSize, sw.reset)
}

object BSW {
  def apply(maxSize: Int, limit: Double, timeWindow: Duration): BSW =
    BSW(maxSize, SlidingWindow(limit, timeWindow), None)
  def apply(maxSize: Int, sw: SlidingWindow): BSW = BSW(maxSize, sw)

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

object ScalaCheckTest extends Properties("SW") {

  def propLatestEventLast(sw: SlidingWindow, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)
    val expLatestEvent = sw.latestEventTime match {
      case None => dateTime
      case Some(le) => if (le > dateTime) le else dateTime
    }

    "Latest event last" |: {
      nsw.latestEventTime match {
        case None     => Prop.falsified
        case Some(lt) => expLatestEvent =? lt
      }
    }
  }


  def propMoveWindow(sw: SlidingWindow, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)
    val boundary = max(sw.latestEventTime.getOrElse(dateTime), dateTime) - sw.timeWindow

    val (_, inWindow) = ((dateTime, value) :: sw.heap).sorted.partition { case (dt, _) => dt.isBefore(boundary) }

    "Moving window" |: inWindow =? nsw.heap
  }


  def propLimitCheck(sw: SlidingWindow, dateTime: DateTime, value: Double): Prop = {
    val (nsw, chk) = sw.add(dateTime, value)
    val boundary = max(sw.latestEventTime.getOrElse(dateTime), dateTime) - sw.timeWindow

    val expCurrVal =
      if (dateTime.isBefore(boundary)) sw.sum
      else SlidingWindow.heapSum(sw.heap.dropWhile { case (dt, _)  => dt.isBefore(boundary) }) + value

    val expChk: Check = if (expCurrVal > sw.limit) LimitExceeded(expCurrVal) else WithinLimit()

    all(
      "Current value coherent" |: expCurrVal =? nsw.sum,
      "Limit check" |: expChk =? chk
    )
  }

  def propStaticCoherence(sw: SlidingWindow): Prop = all(
    "Time window sane" |: Prop(sw.timeWindow.getMillis >= 0),
    "Heap order sane" |: sw.heap.sorted =? sw.heap,
    "No last event if heap empty" |: sw.heap.isEmpty =? sw.latestEventTime.isEmpty
  )

  def propDynamicCoherence(sw: SlidingWindow, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)

    all(
      "Limits unchanged" |: sw.limit =? nsw.limit,
      "Time window unchanged" |: sw.timeWindow =? nsw.timeWindow
    )
  }

  def propDelMin(sw: SlidingWindow, dateTime: DateTime, value: Double): Prop = {
    val (nsw, maybeElem) = sw.delMin

    // FIXME: any other way to write this property??? => with implication perhaps?
    sw.latestEventTime match {
      case Some(_) => (maybeElem ?= Some(sw.heap.head)) && (sw.heap.tail =? nsw.heap)
      case None => (maybeElem ?= None) && (sw.heap =? nsw.heap)
    }
  }

  val LIMIT = 100d
  val TIME_WIN = 10.seconds.toDuration

  property("SWprops") = forAll (genSW(LIMIT, TIME_WIN)) { swInit =>
    val eventsTimeWin = Duration.millis(3 * TIME_WIN.getMillis)
    forAll(genEvents(DateTime.now, eventsTimeWin)){ ls =>
      //property("allProps") = forAll(genOneEvent) { ls =>
      val (_, xs) = ls.foldLeft((swInit, List[Prop]())) { case ((sw, props), (dt, v)) =>
        val prop = all(
          propLimitCheck(sw, dt, v),
          propLatestEventLast(sw, dt, v),
          propMoveWindow(sw, dt, v),
          propDynamicCoherence(sw, dt, v),
          propStaticCoherence(sw),
          propDelMin(sw, dt, v)
        )

        val (nsw, _) = sw.add(dt, v)
        (nsw, props :+ prop)
      }

      all(xs: _*)
    }
  }


  //----------------------------------------------------------------------------------------
  //                                      BSW Properties
  //----------------------------------------------------------------------------------------


  def propSizeExceeded(bsw: BSW, dateTime: DateTime, value: Double): Prop = {
    val (nbsw, eCheck) = bsw.add(dateTime, value)
//    println("CHK: " + eCheck)

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
          //case Left(SizeExceeded(_)) => true // specify what... time conditions
        }
      },
      "Size above max" |: bsw.size > bsw.maxSize ==> Prop.falsified
    )
  }

  def propInvTillMonoIncr(bsw: BSW, dateTime: DateTime, value: Double): Prop = {
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


  def propMaxSizeZero(bsw: BSW, dateTime: DateTime, value: Double): Prop = {
    val (nbsw, eCheck) = bsw.add(dateTime, value)

    atLeastOne(
      "Max size zero" |: bsw.maxSize <= 0 ==> {
        eCheck match {
          case Left(SizeExceeded(_)) => true
          case _ => false
        }
      },
      "Max size above zero" |: bsw.maxSize > 0 ==> Prop(true) // FIXME: ok?
    )
  }


  val EVENTS_TIME_WIN = TIME_WIN + TIME_WIN

  property("BSWprops") = forAll(genEvents(DateTime.now, TIME_WIN)) { ls =>
    //println("==============")

    val (_, xs) = ls.foldLeft((BSW(0, LIMIT, EVENTS_TIME_WIN), List[Prop]())) { case ((bsw, props), (dt, v)) =>
      val prop = all(
        propSizeExceeded(bsw, dt, v),
        propInvTillMonoIncr(bsw, dt, v),
        propMaxSizeZero(bsw, dt, v)
//        propLimitCheck(bsw, dt, v),
//        propLatestEventLast(bsw, dt, v),
//        propMoveWindow(bsw, dt, v),
//        propDynamicCoherence(bsw, dt, v),
//        propStaticCoherence(bsw),
//        propDelMin(bsw, dt, v)
      )

      val (nsw, _) = bsw.add(dt, v)
      (nsw, props :+ prop)
    }

    all(xs: _*)
  }

  def genOneEvent: Gen[List[(DateTime, Double)]] =
    Gen.const(List(
       (new DateTime("1970-01-01T01:50:40.826+01:00"), 60.0),
       (new DateTime("1970-01-01T20:32:53.975+01:00"), 98.0),
       (new DateTime("1970-01-01T06:16:27.104+01:00"), 23.0)
    ))

  def genEvents(start: DateTime, timeSpan: Duration): Gen[List[(DateTime, Double)]] =
    Gen.nonEmptyListOf(
      Gen.zip(
        Gen.choose(0L, timeSpan.getMillis) map { ms => start + new Duration(ms) },
        Gen.choose(0L, 100L) map { l => l.toDouble })) // FIXME: Check fractional addition...

  def genSW(limit: Double, timeWindow: Duration): Gen[SlidingWindow] = for {
    lt <- Gen.choose(0L, limit.toLong) map { l => l.toDouble } // FIXME fractions...
    tw <- Gen.choose(0L, timeWindow.getMillis) map { ms => new Duration(ms) }
  } yield SlidingWindow(limit = lt, timeWindow = tw)


  def max(dt1: DateTime, dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2
}
*/