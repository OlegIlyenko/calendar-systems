package calendar.base

import calendar.RefDate

/**
 * date interface for the corresponding calendar
 */
abstract class Date[+D <: Date[D]] {
  self: D =>

  def calendar: Calendar[D]

  /**
   * is this Date a valid Date
   * like 12.02.31 with yy.mm.dd is not
   * @return true when this Date belongs to the Calendar
   */
  def isValid: Boolean

  /**
   * get the corresponding date in RefCal
   * @return
   */
  def toRef: RefDate

  /**
   * date + time to Add
   * @param toAdd time to add
   * @return a date + the time to add
   */
  // fixme causes stackoverflows (because of fastCalendarCreator)
  def defaultAdd[S <: Date[S]](toAdd: DateElement[S]): D = {
    val refDate = toRef
    calendar.fromRef(new RefDate(refDate.seconds + toAdd.toSecondsForAddition(refDate)))
  }
  def +[S <: Date[S]](toAdd : DateElement[S]) : D

  /**
   * date - time to subtract
   * @param toSub time to subtract
   * @return     a date - time to subtract
   */
  // fixme causes stackoverflows
  def defaultSub[S <: Date[S]](toSub: DateElement[S]): D = {
    val refDate = toRef
    calendar.fromRef(new RefDate(refDate.seconds - toSub.toSecondsForSubtraction(refDate)))
  }
  def - [S <: Date[S]](toSub : DateElement[S]) : D

  /**
   * correct invalid date to the next valid date
   * @return
   */
  def unary_!+ : D

  /**
   * correct invalid date to the next valid date
   * @return
   */
  def unary_!- : D

  def toSeconds: BigInt = toRef.seconds

  def ==[S <: Date[S]](that: S): Boolean = toRef == that.toRef

  def >[S <: Date[S]](that: S): Boolean = toRef > that.toRef

  def >=[S <: Date[S]](that: S): Boolean = toRef >= that.toRef

  def <[S <: Date[S]](that: S): Boolean = toRef < that.toRef

  def <=[S <: Date[S]](that: S): Boolean = toRef <= that.toRef

  def !=[S <: Date[S]](that: S): Boolean = toRef != that.toRef
}
