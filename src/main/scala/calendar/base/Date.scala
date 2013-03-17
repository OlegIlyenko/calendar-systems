package calendar.base

/**
 * The Date interface
 *
 * Here all the sugar is defined
 */
trait Date {
  type D <: Date

  def toCalendar[A](implicit evidence: DateConverter[D, A]) = evidence.convert(this.asInstanceOf[D])

  def +[T <: Date](elem: DateElement[T])(implicit back: DateConverter[T, D], to: DateConverter[D, T]): D = {
    back.convert(to.convert(this).add(elem))
  }

  def -[T <: Date](elem: DateElement[T])(implicit back: DateConverter[T, D], to: DateConverter[D, T]): D = {
    back.convert(to.convert(this).sub(elem))
  }

  def <[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) =
    me.convert(this).smaller(it.convert(date))

  def >[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) =
    me.convert(this).greater(it.convert(date))

  def <=[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) = {
    val i = me.convert(this)
    val that = it.convert(date)
    i.smaller(that) || i.equal(that)
  }

  def >=[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) = {
    val i = me.convert(this)
    val that = it.convert(date)
    i.greater(that) || i.equal(that)
  }

  def ==[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) =
    me.convert(this).equal(it.convert(date))

  /**
   * @return seconds that differ between the other date
   */
  def diff[T <: Date](date : T)(implicit me : DateConverter[D, RefDate], it : DateConverter[T, RefDate]) =
    me.convert(this).millis.millis - it.convert(date).millis.millis


  def add(elem: DateElement[D]): D

  def sub(elem: DateElement[D]): D
}


/**
 * default converters are defined here.
 *
 * the converter magic is also happening here.
 */
object Date {
  implicit def identityConverter[T <: Date] = new DateConverter[T, T] {
    def convert(a: T) = a
  }

  implicit def standardConverter[A <: Date, B <: Date]
  (implicit ev1: DateConverter[A, RefDate], ev2: DateConverter[RefDate, B]) = new DateConverter[A, B] {
    def convert(a: A) = ev2.convert(ev1.convert(a))
  }

}



