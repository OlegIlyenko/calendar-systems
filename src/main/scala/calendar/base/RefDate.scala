package calendar.base

/**
 * Date holding milliseconds since
 *
 * seconds since midnight, January 1, 1970 UTC
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class RefDate(millis : Millisecond) extends Date {
  type C = RefDate

}

case class Millisecond(millis : BigInt) extends DateElement
