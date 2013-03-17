package calendar.base

/**
 * Date holding milliseconds since
 *
 * seconds since midnight, January 1, 1970 UTC
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class RefDate(millis : Millisecond) extends Date {
  type D = RefDate

  def add(elem: DateElement[RefDate#D]) = elem match {
    case Millisecond(m) => millis.millis + m
  }

  def sub(elem: DateElement[RefDate#D]) = elem match {
    case Millisecond(m) => millis.millis - m
  }

  def greater(date : RefDate) = millis.millis > date.millis.millis
  def smaller(date : RefDate) = millis.millis < date.millis.millis
  def equal(date : RefDate) = millis.millis == date.millis.millis

}

object RefDate{
  def as[T <: Date](date : T)(implicit ev : DateConverter[T, RefDate]) = ev.convert(date)
}

case class Millisecond(millis : BigInt) extends DateElement[RefDate]
