package calendar.base

/**
 * Converts a Date from one Date to another
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait DateConverter[A,B] {
  def convert(a : A) : B
}
