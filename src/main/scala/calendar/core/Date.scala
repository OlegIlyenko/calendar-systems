package calendar.core

import calendar.base.RefDate


/**
 * a point on the time axe
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait Date[D <: Date[D]] {
  self: D =>

  def +[E <: DateElement[E]](e: E)(implicit op: DateOp[D, E]) = op.add(Date.this, e)

  def -[E <: DateElement[E]](e: E)(implicit op: DateOp[D, E]) = op.add(Date.this, e.neg)


}


/**
 * standard implicit elements holder for standard behavior and fallback
 */
object Date {
  /**
   * converter for identity (maybe not needed)
   */
  implicit def identityConverter[T <: Date[T]] = new DateConverter[T, T] {
    def convert(a: T) = a
  }

  /**
   * creates operators if no fitting Operator is found.
   * we implicitly transform the date object to a fitting target date.
   */
  implicit def transformOp[A <: Date[A], E <: DateElement[E], B <: Date[B], C <: DateConnect[B, E]]
  (implicit
   transformerA: DateConverter[A, B],
   transformerB: DateConverter[B, A],
   op: DateOp[B, E]) = new DateOp[A, E] {
    def add(a: A, e: E) = transformerB.convert(op.add(transformerA.convert(a), e))
  }

  /**
   * standard converter
   * @todo : move me to calendar.base package (to make it more modular)
   */
  implicit def standardConverter[A <: Date[A], B <: Date[B]]
  (implicit ev1: DateConverter[A, RefDate], ev2: DateConverter[RefDate, B]) = new DateConverter[A, B] {
    def convert(a: A) = ev2.convert(ev1.convert(a))
  }

  /**
   * standard compare object.
   * @todo : move to calendar.base package (to make it more modular)
   */
  implicit def standardCompare[A <: Date[A], B <: Date[B]]
  (implicit transformA: DateConverter[A, RefDate], transformB: DateConverter[B, RefDate]) = new DateCompare[A, B] {

    def equal(a: A, b: B): Boolean = transformA.convert(a).millis == transformB.convert(b).millis

    def less(a: A, b: B): Boolean = transformA.convert(a).millis < transformB.convert(b).millis

    def greater(a: A, b: B): Boolean = transformA.convert(a).millis > transformB.convert(b).millis

    def lessEqual(a: A, b: B): Boolean = transformA.convert(a).millis <= transformB.convert(b).millis

    def greaterEqual(a: A, b: B): Boolean = transformA.convert(a).millis >= transformB.convert(b).millis
  }


}



