package calendar.core


/**
 * For comparison functions
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
trait DateCompare[A <: Date[A], B <: Date[B]] {
  def equal(a: A, b: B): Boolean

  def less(a: A, b: B): Boolean

  def greater(a: A, b: B): Boolean

  def lessEqual(a: A, b: B): Boolean

  def greaterEqual(a: A, b: B): Boolean

  // todo we need something better, like DateElement between dateA dateB
  // so the implicit magic can go on.
  //def diff(a: A, b: B): DateInterval

}

/**
 * for a faster development
 */
trait LazyCompare[A <: Date[A], B <: Date[B]] extends DateCompare[A, B] {
  def greater(a: A, b: B): Boolean = if (equal(a, b)) false else !less(a, b)

  def lessEqual(a: A, b: B): Boolean = equal(a, b) || less(a, b)

  def greaterEqual(a: A, b: B): Boolean = equal(a, b) || greater(a, b)
}
