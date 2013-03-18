package calendar.base

import calendar.util.FlowHelper._

/**
 * Converts a Date from one Date to another
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait Convert[A, B] {
  def convert(a : A) : B
}

object Convert {
  def apply[A, B](convertFn: A => B) = new Convert[A, B] {
    def convert(a: A) = a |> convertFn
  }

  implicit def autoConvert[A, B](a: A)(implicit ev: Convert[A, B]) = ev.convert(a)
}