package calendar.core

/**
 * Converts a Date from one Date to another
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait DateTransformer[A, B] {
  def convert(a: A): B
}

