package calendar.compare

import calendar.core.{LazyCompare, DateCompare}

/**
 * This date is for test false behavior
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class FuckUpDate(i: Int)

object FuckUpDate {
  implicit val equalShift = new LazyCompare[FuckUpDate, ShiftTenDate] {
    def equal(a: FuckUpDate, b: ShiftTenDate): Boolean = a.i == b.i

    def less(a: FuckUpDate, b: ShiftTenDate): Boolean = a.i < b.i
  }

}
