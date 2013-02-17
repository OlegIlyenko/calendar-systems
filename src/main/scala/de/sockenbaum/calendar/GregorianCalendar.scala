package calendar

import common._
import calendar.base._
import util.FastCalendarCreator
import annotation.tailrec
import sun.util.calendar.Gregorian

/**
 * We interpret the Gregorian Calendar as a Calendar system that has no starting point.
 * <p>
 * The Gregorian Calendar has no switch on the 4.Oct.1582 to 15.Oct.1582
 * </p>
 *
 * @author Ingolf Wagner <ingolf.wagner@zalando.de>
 */
object GregorianCalendar extends Calendar[GregorianDate] {

  def create(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int) =
    new GregorianDate(Year(year), Month(month), Day(day), Hour(hour), Minute(minute), Second(second))

  /**
   * transform a RefCal Date to D
   * @param r a RefCal Date
   * @return
   */
  def fromRef(r: RefDate): GregorianDate = fromRefHelper(r)

  val zeroDate = new GregorianDate(Year(1970), Month(1), Day(1), Hour(0), Minute(0), Second(0))
  val fromRefHelper = ((new FastCalendarCreator(zeroDate))
    -> Second(1)
    -> Minute(1)
    -> Hour(1)
    -> Day(1)
    -> Month(1)
    -> Year(1)
    ).finished

  def isLeapYear(year: Int): Boolean = {
    if (year % 4 == 0)
      if (year % 100 == 0)
        year % 400 != 0
      else
        true
    else
      false
  }

  /**
   * number of leap years in the interval (including start and stop year)
   * @param yearStart of starting year
   * @param yearStop of ending year
   * @return number of leap years
   */
  def numberOfLeapYears(yearStart: Int, yearStop: Int): Int =
    if (yearStart == yearStop) 0
    else if (yearStart > yearStop)
      numberOfLeapYears(yearStop, yearStart)
    else
      (yearStart to yearStop).filter(year => isLeapYear(year)).size
}


abstract class GregorianDateElement extends DateElement[GregorianDate]{
  def calendar = GregorianCalendar
}

case class Year(y: Int) extends GregorianDateElement {
  // 366 Days
  val leapYear: BigInt = 31622400
  // 365 Days
  val leapLessYear: BigInt = 31536000

  /**
   * to seconds since 1970 to the start of the year
   * @return
   */
  def toRefSeconds = {
    val years = y - 1970
    val leapYears = if (years > 0) GregorianCalendar.numberOfLeapYears(1970, y - 1)
    else GregorianCalendar.numberOfLeapYears(y, 1970 - 1)
    leapLessYear * (years - leapYears) + leapYear * leapYears
  }

  def isLeap = GregorianCalendar.isLeapYear(y)
}

case class Month(m: Int) extends GregorianDateElement {
  val month31: BigInt = 2678400
  val month30: BigInt = 2592000
  val month29: BigInt = 2505600
  val month28: BigInt = 2419200


  def toRefSeconds(isLeap: Boolean): BigInt =
    if (m == 1) 0
    else Day(1).toSeconds * (1 until m).map(a => Month(a).daysOfMonth(isLeap)).reduce((a, b) => a + b)


  /**
   * number of days in the actual month
   * @return
   */
  def daysOfMonth(isLeap: Boolean): Int = m % 12 match {
    case 1 => 31
    case 2 => if (isLeap) 29 else 28
    case 3 => 31
    case 4 => 30
    case 5 => 31
    case 6 => 30
    case 7 => 31
    case 8 => 31
    case 9 => 30
    case 10 => 31
    case 11 => 30
    case 0 => 31
  }
}

case class Day(d: Int) extends GregorianDateElement {
  val oneDay: BigInt = 86400

  def toSeconds = oneDay * d
}

case class Hour(h: Int) extends GregorianDateElement {
  val oneHour: BigInt = 3600

  def toSeconds = oneHour * h
}

case class Minute(m: Int) extends GregorianDateElement {
  val oneMinute: BigInt = 60

  def toSeconds = oneMinute * m
}

case class Second(s: Int) extends GregorianDateElement {
  def toSeconds: BigInt = s
}

class GregorianDate(val year: Year, val month: Month, val day: Day, val hour: Hour, val minute: Minute, val second: Second) extends Date[GregorianDate] {
  def calendar = GregorianCalendar

  /**
   * is this Date a valid Date
   * like 12.02.31 with yy.mm.dd is not
   * @return true when this Date belongs to the Calendar
   */
  def isValid = ???

  /**
   * get the corresponding date in RefCal
   * @return
   */
  // fixme : does not work for dates before 1970
  def toRef = {
    val monthSec = month.toRefSeconds(isLeap)
    // first day is 1 so we don't want to count it
    val daySec = Day(day.d - 1).toSeconds
    new RefDate(year.toRefSeconds + monthSec + daySec + hour.toSeconds + minute.toSeconds + second.toSeconds)
  }


  /**
   * date + time to Add
   * @param toAdd time to add
   * @return a date + the time to add
   */
  override def +[S <: Date[S]](toAdd: DateElement[S]) = toAdd match {
    case Year(y) => addYear(y)
    case Month(m) => addMonth(m)
    case Day(d) => addDay(d)
    case Hour(h) => addHour(h)
    case Minute(m) => addMinute(m)
    case Second(s) => addSecond(s)
    case _ => defaultAdd(toAdd)
  }


  /**
   * date - time to subtract
   * @param toSub time to subtract
   * @return     a date - time to subtract
   */
  override def -[S <: Date[S]](toSub: DateElement[S]) = toSub match {
    case Year(y) => subYear(y)
    case Month(m) => subMonth(m)
    case Day(d) => subDay(d)
    case Hour(h) => subHour(h)
    case Minute(m) => subMinute(m)
    case Second(s) => subSecond(s)
    case _ => defaultSub(toSub)
  }

  def addYear(y: Int): GregorianDate = {
    GregorianCalendar.create(year.y + y, month.m, day.d, hour.h, minute.m, second.s)
  }

  def subYear(y: Int): GregorianDate = {
    GregorianCalendar.create(year.y - y, month.m, day.d, hour.h, minute.m, second.s)
  }

  def addMonth(toAdd: Int): GregorianDate = if (toAdd == 0) this
  else if (toAdd < 0) subMonth(-toAdd)
  else {
    val resultMonth = month.m + toAdd
    if (resultMonth > 12)
      GregorianCalendar.create(year.y, 1, day.d, hour.h, minute.m, second.s).addYear(1).addMonth(resultMonth - 13)
    else
      GregorianCalendar.create(year.y, resultMonth, day.d, hour.h, minute.m, second.s)
  }


  @tailrec
  final def subMonth(toSub: Int): GregorianDate = if (toSub == 0) this
  else if (toSub < 0) addMonth(-toSub)
  else {
    val resultMonth = month.m - toSub
    if (resultMonth < 1)
      GregorianCalendar.create(year.y, 12, day.d, hour.h, minute.m, second.s).subYear(1).subMonth(-resultMonth)
    else
      GregorianCalendar.create(year.y, resultMonth, day.d, hour.h, minute.m, second.s)
  }


  @tailrec
  final def addDay(toAdd: Int): GregorianDate = if (toAdd == 0) this
  else if (toAdd < 0) subDay(-toAdd)
  else {
    val m = month.daysOfMonth(isLeap)
    val resultDay = day.d + toAdd
    if (resultDay > m)
      GregorianCalendar.create(year.y, month.m, 1, hour.h, minute.m, second.s).addMonth(1).addDay(resultDay - m - 1)
    else
      GregorianCalendar.create(year.y, month.m, resultDay, hour.h, minute.m, second.s)
  }


  @tailrec
  final def subDay(toSub: Int): GregorianDate = if (toSub == 0) this
  else if (toSub < 0) addDay(-toSub)
  else {
    val resultDay = day.d - toSub
    if (resultDay < 1)
      GregorianCalendar.create(year.y, month.m, Month(month.m - 1).daysOfMonth(isLeap), hour.h, minute.m, second.s).subMonth(1).subDay(-resultDay - 1)
    else
      GregorianCalendar.create(year.y, month.m, resultDay, hour.h, minute.m, second.s)
  }

  @tailrec
  final def addHour(toAdd: Int): GregorianDate = if (toAdd == 0) this
  else if (toAdd < 0) subDay(-toAdd)
  else {
    val resultHour = hour.h + toAdd
    if (resultHour > 23)
      GregorianCalendar.create(year.y, month.m, day.d, 0, minute.m, second.s).addDay(1).addHour(resultHour - 24)
    else
      GregorianCalendar.create(year.y, month.m, day.d, resultHour, minute.m, second.s)
  }

  @tailrec
  final def subHour(toSub: Int): GregorianDate = if (toSub == 0) this
  else if (toSub < 0) addHour(-toSub)
  else {
    val resultHour = hour.h - toSub
    if (resultHour < 0)
      GregorianCalendar.create(year.y, month.m, day.d, 23, minute.m, second.s).subDay(1).subHour(-resultHour)
    else
      GregorianCalendar.create(year.y, month.m, day.d, resultHour, minute.m, second.s)
  }

  @tailrec
  final def addMinute(toAdd: Int): GregorianDate = if (toAdd == 0) this
  else if (toAdd < 0) subMinute(-toAdd)
  else {
    val resultMinute = minute.m + toAdd
    if (resultMinute > 59)
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, 0, second.s).addHour(1).addMinute(resultMinute - 60)
    else
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, resultMinute, second.s)
  }

  @tailrec
  final def subMinute(toSub: Int): GregorianDate = if (toSub == 0) this
  else if (toSub < 0) addMinute(-toSub)
  else {
    val resultMinute = minute.m - toSub
    if (resultMinute < 0)
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, 59, second.s).subHour(1).subMinute(-resultMinute)
    else
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, resultMinute, second.s)
  }

  @tailrec
  final def addSecond(toAdd: Int): GregorianDate = if (toAdd == 0) this
  else if (toAdd < 0) subSecond(-toAdd)
  else {
    val resultSecond = second.s + toAdd
    if (resultSecond > 59)
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, minute.m, 0).addMinute(1).addSecond(resultSecond - 60)
    else
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, minute.m, resultSecond)

  }

  @tailrec
  final def subSecond(toSub: Int): GregorianDate = if (toSub == 0) this
  else if (toSub < 0) addSecond(-toSub)
  else {
    val resultSecond = second.s - toSub
    if (resultSecond < 0)
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, minute.m, 59).subMinute(1).subSecond(-resultSecond)
    else
      GregorianCalendar.create(year.y, month.m, day.d, hour.h, minute.m, resultSecond)

  }


  /**
   * correct invalid date to the next valid date
   * @return
   */
  def unary_!+ = ???

  /**
   * correct invalid date to the next valid date
   * @return
   */
  def unary_!- = ???


  override def toString = year.y + "." + month.m + "." + day.d + " " + hour.h + ":" + minute.m + ":" + second.s

  def isLeap: Boolean = GregorianCalendar.isLeapYear(year.y)
}
