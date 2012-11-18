package calendar.base

import calendar.RefDate
import java.lang.IllegalArgumentException

/**
 * The Calendar interface
 */
trait Calendar[D <: Date[D]] {
  /**
   * transform a RefCal Date to D
   * @param r a RefCal Date
   * @return
   */
  def fromRef(r: RefDate): D

}

/**
 * date interface for the corresponding calendar
 */
abstract class Date[D <: Date[D]] {
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
  def defaultAdd(toAdd: DateElement): D = calendar.fromRef(new RefDate(toRef.seconds + toAdd.toSecondsForAddition(this)))
  def +(toAdd : DateElement) : D

  /**
   * date - time to subtract
   * @param toSub time to subtract
   * @return     a date - time to subtract
   */
  // fixme causes stackoverflows
  def defaultSub(toSub: DateElement): D = calendar.fromRef(new RefDate(toRef.seconds - toSub.toSecondsForSubtraction(this)))
  def -(toSub : DateElement) : D

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

  /**
   * creat a Sequence of Dates
   * @param stop stop date
   * @param stepSize stepSize
   * @return
   */
  def to(implicit stepSize: DateElement, stop: D): Iterable[D] =
    if (!stop.isValid)
      throw new IllegalArgumentException(stop + " is not valid")
    else
      to(stepSize, (date => stop > date))


  /**
   * creates an iterable until the predicate becomes false.
   * @param stepSize for the iterable
   * @param p predicate for creating a next element
   * @return
   */
  def to(implicit stepSize: DateElement, p: D => Boolean): Iterable[D] = {

    val me: D = this
    new Iterable[D] {
      def iterator = new Iterator[D] {
        var actual: D = me

        // todo optimize me
        def hasNext = !p(actual + stepSize)

        // todo optimize me
        def next() = {
          if (hasNext) {
            actual = actual + stepSize
            actual
          } else {
            throw new IndexOutOfBoundsException
          }
        }
      }
    }
  }

  /**
   * creates an infinite iterable
   * @param stepSize
   * @return
   */
  def iterable(implicit stepSize: DateElement): Iterable[D] = to(stepSize, _ => true)

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
trait DateElement {
  /**
   * correction function for subtraction
   * this function is meant to be overridden.
   * it returns the seconds of that are needed to
   * @param date
   * @return
   */
  def toSecondsForSubtraction[D <: Date[D]](date: D): BigInt = date.toSeconds - (date - this).toSeconds

  /**
   * correction function for subtraction
   * this function is meant to be overridden
   * @param date
   * @return
   */
  def toSecondsForAddition[D <: Date[D]](date: D): BigInt = (date + this).toSeconds - date.toSeconds
}
