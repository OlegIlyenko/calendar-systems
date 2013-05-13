package calendar.core

/**
 * Element for Calendar arithmetic. Elements will be interpreted as Intervals on the time-axe.
 * When you create your own DateElements you have to register them in you Date Object
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait DateElement[E <: DateElement[E]] {
  def neg: E
}
