package calendar.core

/**
 * Operator type
 *
 * @todo create easy to use creator functions
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait DateOp[D <: Date[D], E <: DateElement[E]] {
  def add(a: D, e: E): D
}

trait PreferredOperator[D <: Date[D], E <: DateElement[E]]
