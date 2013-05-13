package calendar


import calendar.core._

/**
 * Date holding milliseconds since
 *
 * seconds since midnight, January 1, 1970 UTC
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
package object base {

  // -------------------- date objects

  case class RefDate(millis: BigInt) extends Date[RefDate]

  case class Millisecond(millis: BigInt) extends DateElement[Millisecond] {
    def neg: Millisecond = Millisecond(-millis)
  }

  implicit val millisecond = new DateConnect[RefDate, Millisecond]


  // --------------------- static configuration
  object RefDate {
    def as[T <: Date[T]](date: T)(implicit ev: DateConverter[T, RefDate]) = ev.convert(date)
  }


}
