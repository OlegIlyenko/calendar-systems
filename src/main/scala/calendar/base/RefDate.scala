package calendar.base

import Date._

/**
 * Date holding milliseconds since
 *
 * seconds since midnight, January 1, 1970 UTC
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class RefDate(millis : Millisecond){
  def greater(date : RefDate) = millis.millis > date.millis.millis
  def smaller(date : RefDate) = millis.millis < date.millis.millis
  def equal(date : RefDate) = millis.millis == date.millis.millis
}

object RefDate {
  def apply[T : ToRefDateConvert](date : T): RefDate = implicitly[ToRefDateConvert[T]] convert date

  implicit object RefDateTC extends Date[RefDate]
}
