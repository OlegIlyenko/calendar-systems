package calendar

import org.scalatest.FlatSpec

class ReferenceCalendarSuite extends FlatSpec {
  "A ReferenceCalendar" should  "always transform correct" in{
    assert(RefDate$.fromRef(new RefDate(123)) == new RefDate(123))
    assert(RefDate$.from(123) == new RefDate(123))
    assert(RefDate$.from(123) == RefDate$.fromRef(new RefDate(123)))
  }

  it should "implement correct relation functions" in {
    val d1 = new RefDate(1)
    val d2 = new RefDate(2)
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

  it should "not end up in stackoverflows" in {
    val date1 = GregorianDate$.create(2000, 6, 6, 3, 2, 1)
    val date2 = GregorianDate$.create(2000, 6, 5, 3, 2, 1)
    try{
      assert(date1.toRef == (date2.toRef + Day(1)))
    } catch {
      case _: Throwable => assert(condition = false, clue = "recursive call on toSecondsForAdd")
    }
  }


}
