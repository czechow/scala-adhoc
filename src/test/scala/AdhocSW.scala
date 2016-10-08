
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

case class SW(limit: Double = 100d,
              timeWindow: Duration = new Duration(1000L * 10), // 10.seconds,
              heap: List[(DateTime, Double)] = List()) {

  def add(dateTime: DateTime, value: Double): (SW, Check) = {
    val boundary = latestEventTime match {
      case None     => dateTime - timeWindow
      case Some(le) => (if (le > dateTime) le else dateTime) - timeWindow
    }

    val newHeap = ((dateTime, value) :: heap).filter { case (dt, _) => ! dt.isBefore(boundary) }.sorted

    val currVal = SW.heapSum(newHeap)
    val check = if (currVal <= limit) WithinLimit() else LimitExceeded(currVal)

    (this.copy(heap = newHeap), check)
  }

  def sum: Double = SW.heapSum(heap)

  def latestEventTime: Option[DateTime] = heap match {
    case Nil => None
    case xs => Some(xs.max._1)
  }

  def delMin: (SW, (Option[(DateTime, Double)])) = heap match {
    case Nil => (this, None)
    case (x :: xs) => (this.copy(heap = xs), Some(x))
  }

  // FIXME: add here
  def reset: SW = ???

  // FIXME: not tested...

  def size: Int = heap.size
}

object SW {
  def heapSum(heap: List[(DateTime, Double)]): Double =
    heap.foldLeft(0d) { case (acc, (_, v)) => acc + v }
}

//-------------------------------------------------------------------
//                               BSW
//-------------------------------------------------------------------


sealed trait ArbDateTime
final case class BoundDateTime(dt: DateTime) extends ArbDateTime
final case class UndefinedDateTime() extends ArbDateTime


// FIXME: rework to include "recovery" phase...
final case class SizeExceeded(invalidTill: DateTime)




case class BSW private (maxSize: Int, sw: SW, invalidTill: Option[DateTime]) {

  def add(dateTime: DateTime, value: Double): (BSW, Either[SizeExceeded, Check]) = {
    val (nsw, check) = sw.add(dateTime, value)

    if (nsw.heap.size <= maxSize)
      invalidTill match {
        case None => (this.copy(sw = nsw), Right(check))
        case Some(invTill) =>
          if (dateTime.isAfter(invTill)) (this.copy(sw = nsw, invalidTill = None), Right(check))
          else (this.copy(sw = nsw), Left(SizeExceeded(invTill)))
      }
    else {
      val (tnsw, delEl) = BSW.dropToSize(nsw, maxSize)(None)

      val nInvTill = (delEl, invalidTill) match {
        case (Some((dt, _)), Some(invTill)) => BSW.max(dt + nsw.timeWindow, invTill)
        case (Some((dt, _)), None) => dt + nsw.timeWindow
        case (None, Some(invTill)) => invTill
        case (None, None) => new DateTime("9999-12-31")
      }

      (this.copy(sw = tnsw, invalidTill = Some(nInvTill)), Left(SizeExceeded(nInvTill)))
    }
  }

  def size: Int = sw.size

  def reset: BSW = BSW(this.size)
}

object BSW {
  def apply(maxSize: Int): BSW = BSW(maxSize, SW(), None)

  // FIXME: this should be generic...
  def max(dt1: DateTime, dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2

  @tailrec
  private def dropToSize(sw: SW, size: Int)(mEl: Option[(DateTime, Double)]): (SW, Option[(DateTime, Double)]) =
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

  def propLatestEventLast(sw: SW, dateTime: DateTime, value: Double): Prop = {
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


  def propMoveWindow(sw: SW, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)
    val boundary = max(sw.latestEventTime.getOrElse(dateTime), dateTime) - sw.timeWindow

    val (_, inWindow) = ((dateTime, value) :: sw.heap).sorted.partition { case (dt, _) => dt.isBefore(boundary) }

    "Moving window" |: inWindow =? nsw.heap
  }


  def propLimitCheck(sw: SW, dateTime: DateTime, value: Double): Prop = {
    val (nsw, chk) = sw.add(dateTime, value)
    val boundary = max(sw.latestEventTime.getOrElse(dateTime), dateTime) - sw.timeWindow

    val expCurrVal =
      if (dateTime.isBefore(boundary)) sw.sum
      else SW.heapSum(sw.heap.dropWhile { case (dt, _)  => dt.isBefore(boundary) }) + value

    val expChk: Check = if (expCurrVal > sw.limit) LimitExceeded(expCurrVal) else WithinLimit()

    all(
      "Current value coherent" |: expCurrVal =? nsw.sum,
      "Limit check" |: expChk =? chk
    )
  }

  def propStaticCoherence(sw: SW): Prop = all(
    "Time window sane" |: Prop(sw.timeWindow.getMillis >= 0),
    "Heap order sane" |: sw.heap.sorted =? sw.heap,
    "No last event if heap empty" |: sw.heap.isEmpty =? sw.latestEventTime.isEmpty
  )

  def propDynamicCoherence(sw: SW, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)

    all(
      "Limits unchanged" |: sw.limit =? nsw.limit,
      "Time window unchanged" |: sw.timeWindow =? nsw.timeWindow
    )
  }

  def propDelMin(sw: SW, dateTime: DateTime, value: Double): Prop = {
    val (nsw, maybeElem) = sw.delMin

    // FIXME: any other way to write this property??? => with implication perhaps?
    sw.latestEventTime match {
      case Some(_) => (maybeElem ?= Some(sw.heap.head)) && (sw.heap.tail =? nsw.heap)
      case None => (maybeElem ?= None) && (sw.heap =? nsw.heap)
    }
  }


  // FIXME: add SW generator too...
  property("SWprops") = forAll(genEvents(DateTime.now, new Duration(1000L * 10))) { ls =>
  //property("allProps") = forAll(genOneEvent) { ls =>
    val (_, xs) = ls.foldLeft((SW(), List[Prop]())) { case ((sw, props), (dt, v)) =>
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


  def propSizeExceeded(bsw: BSW, dateTime: DateTime, value: Double): Prop = {
    val (nbsw, eCheck) = bsw.add(dateTime, value)
    println("CHK: " + eCheck)

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
    val (nbsw, eCheck) = bsw.add(dateTime, value)

    "InvTill monotonous increase" |: {
      (bsw.invalidTill, nbsw.invalidTill) match {
        case (None, None) => true
        case (None, Some(_)) => true
        case (Some(_), None) => true
        case (Some(invTill), Some(nInvTill)) => invTill <= nInvTill
      }
    }
  }


  property("BSWprops") = forAll(genEvents(DateTime.now, new Duration(1000L * 10))) { ls =>
    println("==============")

    val (_, xs) = ls.foldLeft((BSW(1), List[Prop]())) { case ((bsw, props), (dt, v)) =>
      val prop = all(
        propSizeExceeded(bsw, dt, v),
        propInvTillMonoIncr(bsw, dt, v)
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
//       (new DateTime("1970-01-01T07:13:08.442+01:00"), 17.75046456436249)
//      ,(new DateTime("1970-01-01T09:44:37.218+01:00"), 61.99144631481895)
//      ,(new DateTime("1970-01-01T07:22:24.949+01:00"), 99.33360805077588)
       (new DateTime("1970-01-01T01:50:40.826+01:00"), 60.0),
       (new DateTime("1970-01-01T20:32:53.975+01:00"), 98.0),
       (new DateTime("1970-01-01T06:16:27.104+01:00"), 23.0)
    ))

  def genEvents(start: DateTime, timeSpan: Duration): Gen[List[(DateTime, Double)]] =
    Gen.nonEmptyListOf(
      Gen.zip(
        Gen.choose(0L, timeSpan.getMillis) map { ms => start + new Duration(ms) },
        Gen.choose(0L, 100L) map { l => l.toDouble })) // FIXME: Check fractional addition...

//  def genSW(timeWindow: Duration): Gen[SW] = for {
//    limit <- Gen.choose(0d, 100d)
//    tw <- Gen.choose(0L, timeWindow.getMillis) map { ms => new Duration(ms) }
//  } yield SW(limit = limit, timeWindow = 10.seconds)


  def max(dt1: DateTime, dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2
}
