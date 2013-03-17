package calendar.base

/**
 * Converts a Date from one Calendar to another
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait CalendarConverter[A,B] {
  def convert(a : A) : B
}
