package calendar.base

import calendar.RefDate

/**
 * The Calendar interface
 */
trait Calendar[+D <: Date[D]] {

  /**
   * transform a RefCal Date to D
   * @param r a RefCal Date
   * @return
   */
  def fromRef(r: RefDate): D

  /**
   * transform the date to my date. it's a bit buggy but this will change in the future.
   * It hopes that it will always be called by defaultAdd and defaultSub.
   * @param date
   * @tparam S
   * @return
   */
  def transformToMe[S <: Date[S]](date : S) : D = date match {
    case s : RefDate => fromRef(s.toRef)
    case d : D => d
    case s => fromRef(s.toRef)
  }

}

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

/**
 * date elements like Day, Month, Year
 */
trait DateElement[D <: Date[D]] {

  def calendar : Calendar[D]

  def transformedToMe[S <: Date[S]](date : S) : D =
    calendar.transformToMe(date)

  /**
   * correction function for subtraction
   * this function is meant to be overridden.
   * it returns the seconds of that are needed to
   * @param date
   * @return
   */
  def toSecondsForSubtraction[S <: Date[S]](date: S): BigInt = {
    val myDate = transformedToMe(date)
    myDate.toSeconds - (myDate - this).toSeconds
  }

  /**
   * correction function for subtraction
   * this function is meant to be overridden
   * @param date
   * @return
   */
  def toSecondsForAddition[S <: Date[S]](date: S): BigInt = {
    val myDate = transformedToMe(date)
    //println(date)
    (myDate + this).toSeconds - myDate.toSeconds
  }
}
