import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Prop.AnyOperators

import com.github.nscala_time.time.Imports._

sealed trait Check
final case class WithinLimit() extends Check
final case class LimitExceeded(currVal: Double) extends Check

case class SlidingWindow(limit: Double = 1000d,
                         timeWindow: Duration = 15.minutes,
                         heap: List[(DateTime, Double)] = List()) {

  def add(dateTime: DateTime, value: Double): (SlidingWindow, Check) = {
    val boundary = dateTime - timeWindow
    val newHeap = ((dateTime, value) :: heap).filter { case (dt, _) =>
      ! dt.isBefore(boundary)
    }.sorted

    val check = if (currVal <= limit) WithinLimit() else LimitExceeded(currVal)

    (this.copy(heap = newHeap), check)
  }

  def currVal: Double = heap.foldLeft(0d) { case (acc, (_, v)) => acc + v }
}




object ScalaCheckTest extends Properties("SlidingWindow") {


  def propCurrValCoherent(sw: SlidingWindow, dt: DateTime, value: Double): Prop = {
    val (nsw, _) = sw.add(dt, value)
    "CurrVal coherent" |: sw.currVal + value =? nsw.currVal
  }

  def propLimitCheck(sw: SlidingWindow, dt: DateTime, value: Double): Prop = {
    val (_, check) = sw.add(dt, value)
    val expCurrVal = sw.currVal + value

    "Limit check" |: {
      if (expCurrVal >  sw.limit) check ?= LimitExceeded(expCurrVal)
      else check ?= WithinLimit()
    }
  }

  property("Limit") = forAll(Gen.posNum[Double]) { v =>
    val sw = SlidingWindow()
    all(
      propCurrValCoherent(sw, DateTime.now, v),
      propLimitCheck(sw, DateTime.now, v)
    )
  }
}
