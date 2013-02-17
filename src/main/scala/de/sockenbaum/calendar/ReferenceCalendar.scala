package calendar

import calendar.base._

/**
 * the reference de.sockenbaum.calendar object.
 * seconds since midnight, January 1, 1970 UTC
 * <ul>
 * <li> 1 Minute = 60 Seconds                            </li>
 * <li> 1 Hour = 60 Minute = 3600 Seconds                </li>
 * <li> 1 Day = 24 Hours = 1440 Minute = 86.400 Seconds  </li>
 * </ul>
 */
object RefCalendar extends Calendar[RefDate] {
  def fromRef(r: RefDate) = r.toRef

  def from(seconds: BigInt) = new RefDate(seconds)
}

/**
 * the reference de.sockenbaum.calendar Elements
 */
final class RefDate(val seconds: BigInt) extends Date[RefDate] {
  def calendar = RefCalendar

  def +[S <: Date[S]](toAdd: DateElement[S]) = defaultAdd(toAdd)

  def -[S <: Date[S]](toSub: DateElement[S]) = defaultSub(toSub)

  /**
   * is this Date a valid Date
   * like 12.02.31 with yy.mm.dd is not
   * @return true when this Date belongs to the Calendar
   */
  def isValid = true

  /**
   * get the corresponding date in RefCal
   * @return
   */
  def toRef = new RefDate(seconds)

  /**
   * correct invalid date to the next valid date
   * @return
   */
  def unary_!+ = toRef

  /**
   * correct invalid date to the next valid date
   * @return
   */
  def unary_!- = toRef

  def ==(that: RefDate) = seconds == that.seconds

  def >(that: RefDate) = seconds > that.seconds

  def >=(that: RefDate) = seconds >= that.seconds

  def <(that: RefDate) = seconds < that.seconds

  def <=(that: RefDate) = seconds <= that.seconds

  def !=(that: RefDate) = seconds != that.seconds

  override def toString = "[refdate " + seconds + "]"
}

