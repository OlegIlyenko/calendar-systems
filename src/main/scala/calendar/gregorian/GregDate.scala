package calendar.gregorian

import calendar.base._
import calendar.base.Millisecond
import calendar.util.FastCalendarCreator
import annotation.tailrec

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class GregDate(year: Year, month: Month, day: Day, hour: Hour, min: Minute, sec: Second, mil: Millisecond)
  extends Date {
  type D = GregDate

  def add(elem: DateElement[D]) = elem match {
    case Year(y) => addYear(y)
    case Month(m) => addMonth(m)
    case Day(d) => addDay(d)
    case Hour(h) => addHour(h)
    case Minute(m) => addMinute(m)
    case Second(s) => addSecond(s)
  }

  def delete(elem: DateElement[D]) = elem match {
    case Year(y) => addYear(-y)
    case Month(m) => subMonth(m)
    case Day(d) => subDay(d)
    case Hour(h) => subHour(h)
    case Minute(m) => subMinute(m)
    case Second(s) => subSecond(s)
  }

  def addYear(y: Int): GregDate = {
    GregDate.create(year.y + y, month.m, day.d, hour.h, min.m, sec.s)
  }

  def addMonth(toAdd: Int): GregDate = if (toAdd == 0) this
  else if (toAdd < 0) subMonth(-toAdd)
  else {
    val resultMonth = month.m + toAdd
    if (resultMonth > 12)
      GregDate.create(year.y, 1, day.d, hour.h, min.m, sec.s).addYear(1).addMonth(resultMonth - 13)
    else
      GregDate.create(year.y, resultMonth, day.d, hour.h, min.m, sec.s)
  }


  @tailrec
  final def subMonth(toSub: Int): GregDate = if (toSub == 0) this
  else if (toSub < 0) addMonth(-toSub)
  else {
    val resultMonth = month.m - toSub
    if (resultMonth < 1)
      GregDate.create(year.y, 12, day.d, hour.h, min.m, sec.s).subYear(1).subMonth(-resultMonth)
    else
      GregDate.create(year.y, resultMonth, day.d, hour.h, min.m, sec.s)
  }


  @tailrec
  final def addDay(toAdd: Int): GregDate = if (toAdd == 0) this
  else if (toAdd < 0) subDay(-toAdd)
  else {
    val m = month.daysOfMonth(isLeap)
    val resultDay = day.d + toAdd
    if (resultDay > m)
      GregDate.create(year.y, month.m, 1, hour.h, min.m, sec.s).addMonth(1).addDay(resultDay - m - 1)
    else
      GregDate.create(year.y, month.m, resultDay, hour.h, min.m, sec.s)
  }


  @tailrec
  final def subDay(toSub: Int): GregDate = if (toSub == 0) this
  else if (toSub < 0) addDay(-toSub)
  else {
    val resultDay = day.d - toSub
    if (resultDay < 1)
      GregDate.create(year.y, month.m, Month(month.m - 1).daysOfMonth(isLeap), hour.h, min.m, sec.s).subMonth(1).subDay(-resultDay - 1)
    else
      GregDate.create(year.y, month.m, resultDay, hour.h, min.m, sec.s)
  }

  @tailrec
  final def addHour(toAdd: Int): GregDate = if (toAdd == 0) this
  else if (toAdd < 0) subDay(-toAdd)
  else {
    val resultHour = hour.h + toAdd
    if (resultHour > 23)
      GregDate.create(year.y, month.m, day.d, 0, min.m, sec.s).addDay(1).addHour(resultHour - 24)
    else
      GregDate.create(year.y, month.m, day.d, resultHour, min.m, sec.s)
  }

  @tailrec
  final def subHour(toSub: Int): GregDate = if (toSub == 0) this
  else if (toSub < 0) addHour(-toSub)
  else {
    val resultHour = hour.h - toSub
    if (resultHour < 0)
      GregDate.create(year.y, month.m, day.d, 23, min.m, sec.s).subDay(1).subHour(-resultHour)
    else
      GregDate.create(year.y, month.m, day.d, resultHour, min.m, sec.s)
  }

  @tailrec
  final def addMinute(toAdd: Int): GregDate = if (toAdd == 0) this
  else if (toAdd < 0) subMinute(-toAdd)
  else {
    val resultMinute = min.m + toAdd
    if (resultMinute > 59)
      GregDate.create(year.y, month.m, day.d, hour.h, 0, sec.s).addHour(1).addMinute(resultMinute - 60)
    else
      GregDate.create(year.y, month.m, day.d, hour.h, resultMinute, sec.s)
  }

  @tailrec
  final def subMinute(toSub: Int): GregDate = if (toSub == 0) this
  else if (toSub < 0) addMinute(-toSub)
  else {
    val resultMinute = min.m - toSub
    if (resultMinute < 0)
      GregDate.create(year.y, month.m, day.d, hour.h, 59, sec.s).subHour(1).subMinute(-resultMinute)
    else
      GregDate.create(year.y, month.m, day.d, hour.h, resultMinute, sec.s)
  }

  @tailrec
  final def addSecond(toAdd: Int): GregDate = if (toAdd == 0) this
  else if (toAdd < 0) subSecond(-toAdd)
  else {
    val resultSecond = sec.s + toAdd
    if (resultSecond > 59)
      GregDate.create(year.y, month.m, day.d, hour.h, min.m, 0).addMinute(1).addSecond(resultSecond - 60)
    else
      GregDate.create(year.y, month.m, day.d, hour.h, min.m, resultSecond)

  }

  @tailrec
  final def subSecond(toSub: Int): GregDate = if (toSub == 0) this
  else if (toSub < 0) addSecond(-toSub)
  else {
    val resultSecond = sec.s - toSub
    if (resultSecond < 0)
      GregDate.create(year.y, month.m, day.d, hour.h, min.m, 59).subMinute(1).subSecond(-resultSecond)
    else
      GregDate.create(year.y, month.m, day.d, hour.h, min.m, resultSecond)

  }


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

  implicit def toRef = new DateConverter[GregDate, RefDate] {
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

  val zeroDate = GregDate(Year(1970), Month(1), Day(1), Hour(0), Minute(0), Second(0), Millisecond(0))
  val refHelper = ((new FastCalendarCreator(zeroDate)) -> Second(1) -> Minute(1) -> Hour(1) -> Day(1) -> Month(1) -> Year(1)).finished

  implicit def fromRef = new DateConverter[RefDate, GregDate] {
    def convert(a: RefDate) = refHelper(a)
  }
}
