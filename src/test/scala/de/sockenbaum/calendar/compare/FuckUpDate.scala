package calendar.compare

import calendar.core.{DateTransformer, Date, LazyCompare, DateCompare}
import calendar.base.RefDate

/**
 * This date is for test false behavior
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class FuckUpDate(i: Int) extends Date[FuckUpDate]

object FuckUpDate {
  implicit val equalShift = new LazyCompare[FuckUpDate, ShiftTenDate] {
    def equal(a: FuckUpDate, b: ShiftTenDate): Boolean = a.i == b.i

    def less(a: FuckUpDate, b: ShiftTenDate): Boolean = a.i < b.i
  }

  // --- toRef

  implicit val toRef = new DateTransformer[FuckUpDate, RefDate] {
    def convert(a: FuckUpDate): RefDate = RefDate(a.i)
  }
  implicit val fromRef = new DateTransformer[RefDate, FuckUpDate] {
    def convert(a: RefDate): FuckUpDate = FuckUpDate(a.millis.intValue())
  }
}
