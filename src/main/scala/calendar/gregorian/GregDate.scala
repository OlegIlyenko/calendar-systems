package calendar.gregorian

import calendar.base._
import calendar.base.Millisecond

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class GregDate(year: Year, month: Month, day: Day, hour: Hour, min: Minute, sec: Second, mil: Millisecond)
  extends Date {
  type D = GregDate

  def add(elem: DateElement[GregDate#D]) = null

  def delete(elem: DateElement[GregDate#D]) = null
}


object GregDate {

  def create(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int) =
    GregDate(Year(year), Month(month), Day(day), Hour(hour), Minute(minute), Second(second), Millisecond(0))

  // todo make me milli seconds
  val leapYear: BigInt = 31622400
  val leapLessYear: BigInt = 31536000
  val month31: BigInt = 2678400
  val month30: BigInt = 2592000
  val month29: BigInt = 2505600
  val month28: BigInt = 2419200
  val oneDay: BigInt = 86400
  val oneHour: BigInt = 3600
  val oneMinute: BigInt = 60
  val oneSecond: BigInt = 1


  private def yearToMillisecs(a: GregDate) = {
    val years = a.year.y - 1970
    val leapYears = if (years > 0) numberOfLeapYears(1970, a.year.y - 1)
    else numberOfLeapYears(y, 1970 - 1)
    leapLessYear * (years - leapYears) + leapYear * leapYears
  }

  def numberOfLeapYears(yearStart: Int, yearStop: Int): Int = {
    if (yearStart == yearStop) 0
    else if (yearStart > yearStop)
      numberOfLeapYears(yearStop, yearStart)
    else
      (yearStart to yearStop).filter(year => isLeapYear(year)).size
  }

  def isLeapYear(year: Int): Boolean = {
    if (year % 4 == 0)
      if (year % 100 == 0)
        year % 400 != 0
      else
        true
    else
      false
  }

  private def monthToMillisecs(month: Month, isLeap: Boolean) = {
    if (month.m == 1) 0
    else oneDay * (1 until month.m).map(a => Month(a).daysOfMonth(isLeap)).reduce((a, b) => a + b)
  }

  def toRef = new DateConverter[GregDate, RefDate] {
    def convert(a: GregDate) = RefDate(Millisecond(
      yearToMillisecs(a) +
        monthToMillisecs(a) +
        oneDay * (a.day.d - 1) +
        oneHour * a.hour.h +
        oneMinute * a.min.m +
        oneSecond * a.sec.s +
        a.mil.millis
    ))
  }
}
