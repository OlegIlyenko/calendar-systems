package calendar

import base.{Millisecond, RefDate}
import org.scalatest.FlatSpec
import calendar.base.Date._

class ReferenceCalendarSuite extends FlatSpec {
  "A ReferenceCalendar" should "implement correct relation functions" in {
    val d1 = RefDate(Millisecond(1))
    val d2 = RefDate(Millisecond(2))
    assert(d1 <  d2)
    assert(d1 <= d2)
    assert(d1 == d1)
    assert(d1 != d2)
    assert(d2 >  d1)
    assert(d2 >= d1)
    assert(! (d1 > d2))
    assert(! (d2 < d1))
    assert(! (d1 == d2))
  }

  it should "have a korrect artithmetic" ignore{
    // write me
  }
}
