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

  def ===[A <: Date[A]](that: A)(implicit compare: DateCompare[D, A]) = compare.equal(Date.this, that)

  def !==[A <: Date[A]](that: A)(implicit compare: DateCompare[D, A]) = !compare.equal(Date.this, that)

  def <=[A <: Date[A]](that: A)(implicit compare: DateCompare[D, A]) = compare.lessEqual(Date.this, that)

  def >=[A <: Date[A]](that: A)(implicit compare: DateCompare[D, A]) = compare.greaterEqual(Date.this, that)

  def <[A <: Date[A]](that: A)(implicit compare: DateCompare[D, A]) = compare.less(Date.this, that)

  def >[A <: Date[A]](that: A)(implicit compare: DateCompare[D, A]) = compare.greater(Date.this, that)

}


/**
 * standard implicit elements holder for standard behavior and fallback
 */
object Date {
  /**
   * converter for identity (maybe not needed)
   */
  implicit def identityConverter[T <: Date[T]] = new DateTransformer[T, T] {
    def convert(a: T) = a
  }

  /**
   * creates operators if no fitting Operator is found.
   * we implicitly transform the date object to a fitting target date.
   */
  implicit def transformOp[A <: Date[A], E <: DateElement[E], B <: Date[B], C <: DateConnect[B, E]]
  (implicit
   transformerA: DateTransformer[A, B],
   transformerB: DateTransformer[B, A],
   op: DateOp[B, E]) = new DateOp[A, E] {
    def add(a: A, e: E) = transformerB.convert(op.add(transformerA.convert(a), e))
  }

  /**
   * standard converter
   * @todo : move me to calendar.base package (to make it more modular)
   */
  implicit def standardConverter[A <: Date[A], B <: Date[B]]
  (implicit ev1: DateTransformer[A, RefDate], ev2: DateTransformer[RefDate, B]) = new DateTransformer[A, B] {
    def convert(a: A) = ev2.convert(ev1.convert(a))
  }

  /**
   * standard compare object.
   * @todo move to calendar.base package (to make it more modular)
   * @todo move me lover prioritises to twistCompare
   */
  implicit def standardCompare[A <: Date[A], B <: Date[B]]
  (implicit transformA: DateTransformer[A, RefDate], transformB: DateTransformer[B, RefDate]) = new DateCompare[A, B] {

    def equal(a: A, b: B): Boolean = transformA.convert(a).millis == transformB.convert(b).millis

    def less(a: A, b: B): Boolean = transformA.convert(a).millis < transformB.convert(b).millis

    def greater(a: A, b: B): Boolean = transformA.convert(a).millis > transformB.convert(b).millis

    def lessEqual(a: A, b: B): Boolean = transformA.convert(a).millis <= transformB.convert(b).millis

    def greaterEqual(a: A, b: B): Boolean = transformA.convert(a).millis >= transformB.convert(b).millis
  }

  /**
   * twists the definition of equality, so you don't have to implement both
   */
  implicit def twistCompare[A <: Date[A], B <: Date[B]]
  (implicit twist: DateCompare[A, B]) = new DateCompare[B, A] {
    def equal(a: B, b: A): Boolean = twist.equal(b, a)

    def less(a: B, b: A): Boolean = twist.less(b, a)

    def greater(a: B, b: A): Boolean = twist.greater(b, a)

    def lessEqual(a: B, b: A): Boolean = twist.lessEqual(b, a)

    def greaterEqual(a: B, b: A): Boolean = twist.greaterEqual(b, a)
  }


}



