
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Prop.AnyOperators
import com.github.nscala_time.time.Imports._


sealed trait Check
final case class WithinLimit() extends Check
final case class LimitExceeded(currVal: Double) extends Check

case class SW(limit: Double = 100d,
              timeWindow: Duration = new Duration(1000L * 10), // 10.seconds,
              heap: List[(DateTime, Double)] = List()) {

  def add(dateTime: DateTime, value: Double): (SW, Check) = {
    val boundary = latestEventTime match {
      case None => dateTime - timeWindow
      case Some(le) => (if (le > dateTime) le else dateTime) - timeWindow
    }

    val newHeap = ((dateTime, value) :: heap).filter { case (dt, _) => !dt.isBefore(boundary) }.sorted

    val currVal = SW.heapSum(newHeap)
    val check = if (currVal <= limit) WithinLimit() else LimitExceeded(currVal)

    (this.copy(heap = newHeap), check)
  }

  def latestEventTime: Option[DateTime] = heap match {
    case Nil => None
    case xs => Some(xs.max._1)
  }

  def sum: Double = SW.heapSum(heap)

}


object SW {
  def heapSum(heap: List[(DateTime, Double)]): Double =
    heap.foldLeft(0d) { case (acc, (_, v)) => acc + v }
}


object ScalaCheckTest extends Properties("SW") {

  def propLatestEventLast(sw: SW, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)
    val expLatestEvent = sw.latestEventTime match {
      case None => dateTime
      case Some(le) => if (le > dateTime) le else dateTime
    }

    "Latest event last" |: {
      nsw.latestEventTime match {
        case None => Prop.falsified
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
      else SW.heapSum(sw.heap.dropWhile { case (dt, _) => dt.isBefore(boundary) }) + value

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


  property("allProps") = forAll(genEvents(DateTime.now, new Duration(1000L * 10))) { ls =>
  //property("allProps") = forAll(genOneEvent) { ls =>
    val (_, xs) = ls.foldLeft((SW(), List[Prop]())) { case ((sw, props), (dt, v)) =>
      val prop = all(
        propLimitCheck(sw, dt, v),
        propLatestEventLast(sw, dt, v),
        propMoveWindow(sw, dt, v),
        propDynamicCoherence(sw, dt, v),
        propStaticCoherence(sw)
      )

      val (nsw, _) = sw.add(dt, v)
      (nsw, props :+ prop)
    }

    all(xs: _*)
  }

  def propDynamicCoherence(sw: SW, dateTime: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dateTime, value)

    all(
      "Limits unchanged" |: sw.limit =? nsw.limit,
      "Time window unchanged" |: sw.timeWindow =? nsw.timeWindow
    )
  }

  def genOneEvent: Gen[List[(DateTime, Double)]] =
    Gen.const(List(
      (new DateTime("1970-01-01T01:50:40.826+01:00"), 60.0),
      (new DateTime("1970-01-01T20:32:53.975+01:00"), 98.0),
      (new DateTime("1970-01-01T06:16:27.104+01:00"), 23.0)
    ))


  def genSW(timeWindow: Duration): Gen[SW] = for {
    limit <- Gen.choose(0d, 100d)
    tw <- Gen.choose(0L, timeWindow.getMillis) map { ms => new Duration(ms) }
  } yield
    SW(limit = limit, timeWindow = tw)

  def genEvents(start: DateTime,
                timeSpan: Duration): Gen[List[(DateTime, Double)]] =
    Gen.nonEmptyListOf(
      Gen.zip(
        Gen.choose(0L, timeSpan.getMillis) map { ms => start + new Duration(ms) }, // FIXME: add start?
        Gen.choose(0L, 100L) map { l => l.toDouble }))

  def max(dt1: DateTime,
          dt2: DateTime): DateTime = if (dt1.isAfter(dt2)) dt1 else dt2
}
