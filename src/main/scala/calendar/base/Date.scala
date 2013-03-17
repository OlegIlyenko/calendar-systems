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
    back.convert(to.convert(this.asInstanceOf[D]).add(elem))
  }

  def -[T <: Date](elem: DateElement[T])(implicit back: DateConverter[T, D], to: DateConverter[D, T]): D = {
    back.convert(to.convert(this.asInstanceOf[D]).sub(elem))
  }

  def <[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) =
    me.convert(this.asInstanceOf[D]).smaller(it.convert(date))

  def >[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) =
    me.convert(this.asInstanceOf[D]).greater(it.convert(date))

  def <=[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) = {
    val i = me.convert(this.asInstanceOf[D])
    val that = it.convert(date)
    i.smaller(that) || i.equal(that)
  }

  def >=[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) = {
    val i = me.convert(this.asInstanceOf[D])
    val that = it.convert(date)
    i.greater(that) || i.equal(that)
  }

  def ==[T <: Date](date: T)(implicit me: DateConverter[D, RefDate], it: DateConverter[T, RefDate]) =
    me.convert(this.asInstanceOf[D]).equal(it.convert(date))

  /**
   * @return seconds that differ between the other date
   */
  def diff[T <: Date](date : T)(implicit me : DateConverter[D, RefDate], it : DateConverter[T, RefDate]) =
    me.convert(this.asInstanceOf[D]).millis.millis - it.convert(date).millis.millis


  private def add[T <: Date](elem: DateElement[T]): T

  private def sub[T <: Date](elem: DateElement[T]): T
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



