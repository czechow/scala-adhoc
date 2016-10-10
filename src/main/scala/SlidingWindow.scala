
import SlidingWindow.{Check, LimitExceeded, WithinLimit}
import com.github.nscala_time.time.Imports._
import org.scalacheck.Prop.{AnyOperators, _}
import org.scalacheck._

//import scalaz.Heap
//
//trait Num[A] {
//  def fromInteger(: A
//  def +(that: A)
//}



//-------------------------------------------------------------------
//                           SlidingWindow
//-------------------------------------------------------------------

case class SlidingWindow private (limit: Double,
                                  timeWindow: Duration,
                                  heap: List[(DateTime, Double)] = List()) {

  def add(dateTime: DateTime, value: Double): (SlidingWindow, Check) = {
    val boundary = latestEventTime match {
      case None     => dateTime - timeWindow
      case Some(le) => (if (le > dateTime) le else dateTime) - timeWindow
    }

    val newHeap = ((dateTime, value) :: heap).filter { case (dt, _) => ! dt.isBefore(boundary) }.sorted

    val currVal = SlidingWindow.elemSum(newHeap)
    val check = if (currVal <= limit) WithinLimit() else LimitExceeded(currVal)

    (this.copy(heap = newHeap), check)
  }


  def sum: Double = SlidingWindow.elemSum(heap)

  def latestEventTime: Option[DateTime] = heap match {
    case Nil => None
    case xs => Some(xs.max._1)
  }

  def delMin: (SlidingWindow, (Option[(DateTime, Double)])) = heap match {
    case Nil => (this, None)
    case (x :: xs) => (this.copy(heap = xs), Some(x))
  }

  def reset: SlidingWindow = SlidingWindow(this.limit, this.timeWindow)

  def size: Int = heap.size

  def elems: List[(DateTime, Double)] = heap
}


object SlidingWindow {
  def apply(limit: Double, timeWindow: Duration) = new SlidingWindow(limit, timeWindow)

  def elemSum(heap: List[(DateTime, Double)]): Double =
    heap.foldLeft(0d) { case (acc, (_, v)) => acc + v }

  sealed trait Check
  final case class WithinLimit() extends Check
  final case class LimitExceeded(currVal: Double) extends Check
}

//-------------------------------------------------------------------
//                             Properties
//-------------------------------------------------------------------

object ScalaCheckSWTest extends Properties("SlidingWindow") {

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

    val (_, inWindow) = ((dateTime, value) :: sw.elems).sorted.partition { case (dt, _) => dt.isBefore(boundary) }

    "Moving window" |: inWindow =? nsw.elems
  }

  def propLimitCheck(sw: SlidingWindow, dateTime: DateTime, value: Double): Prop = {
    val (nsw, chk) = sw.add(dateTime, value)
    val boundary = max(sw.latestEventTime.getOrElse(dateTime), dateTime) - sw.timeWindow

    val expCurrVal =
      if (dateTime.isBefore(boundary)) sw.sum
      else SlidingWindow.elemSum(sw.elems.dropWhile { case (dt, _)  => dt.isBefore(boundary) }) + value

    val expChk: Check = if (expCurrVal > sw.limit) LimitExceeded(expCurrVal) else WithinLimit()

    all(
      "Current value coherent" |: expCurrVal =? nsw.sum,
      "Limit check" |: expChk =? chk
    )
  }

  def propStaticCoherence(sw: SlidingWindow): Prop = all(
    "Time window sane" |: Prop(sw.timeWindow.getMillis >= 0),
    "Heap order sane" |: sw.elems.sorted =? sw.elems,
    "No last event if heap empty" |: sw.elems.isEmpty =? sw.latestEventTime.isEmpty
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

    "DelMin" |: {
      sw.latestEventTime match {
        case Some(_) => (maybeElem ?= Some(sw.elems.head)) && (sw.elems.tail =? nsw.elems)
        case None => (maybeElem ?= None) && (sw.elems =? nsw.elems)
      }
    }
  }

  def propReset(sw: SlidingWindow): Prop = {
    val nsw = sw.reset

    all(
      nsw.limit ?= sw.limit,
      nsw.timeWindow ?= sw.timeWindow,
      nsw.size ?= 0
    )
  }

  val LIMIT = 100d
  val TIME_WIN = 10.seconds.toDuration

  property("Props") = forAll (genSW(LIMIT, TIME_WIN)) { swInit =>
    val eventsTimeWin = Duration.millis(3 * TIME_WIN.getMillis)
    forAll(genEvents(DateTime.now, eventsTimeWin)){ ls =>
      val (_, xs) = ls.foldLeft((swInit, List[Prop]())) { case ((sw, props), (dt, v)) =>
        val prop = all(
          propLimitCheck(sw, dt, v),
          propLatestEventLast(sw, dt, v),
          propMoveWindow(sw, dt, v),
          propDelMin(sw, dt, v),
          propDynamicCoherence(sw, dt, v),
          propStaticCoherence(sw),
          propReset(sw)
        )

        val (nsw, _) = sw.add(dt, v)
        (nsw, props :+ prop)
      }

      all(xs: _*)
    }
  }

  //----------------------------------------------------------------------------------------
  //                                      Generators
  //----------------------------------------------------------------------------------------

  def genEvents(start: DateTime, timeSpan: Duration): Gen[List[(DateTime, Double)]] =
    Gen.nonEmptyListOf(
      Gen.zip(
        Gen.choose(0L, timeSpan.getMillis) map { ms => start + new Duration(ms) },
        Gen.choose(0L, 100L) map { l => l.toDouble }))

  def genSW(limit: Double, timeWindow: Duration): Gen[SlidingWindow] = for {
    lt <- Gen.choose(0L, limit.toLong) map { l => l.toDouble }
    tw <- Gen.choose(0L, timeWindow.getMillis) map { ms => new Duration(ms) }
  } yield SlidingWindow(limit = lt, timeWindow = tw)


  def max(dt1: DateTime, dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2
}
