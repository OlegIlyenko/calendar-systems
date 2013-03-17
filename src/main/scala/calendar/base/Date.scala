package calendar.base

/**
 * The Date interface
 *
 * Here all the sugar is defined
 */
trait Date {
  type D <: Date

  def ??? = throw new IllegalStateException()

  def toCalendar[A](implicit evidence: DateConverter[D, A]) =
    evidence.convert(this.asInstanceOf[D])

  def +[T <: Date](elem : DateElement[T])
                  (implicit back : DateConverter[T,D], to: DateConverter[D,T]) : D = {
    back.convert(to.convert(this).add(elem))
  }

  def -[T <: Date](elem : DateElement[T])
                  (implicit back : DateConverter[T,D], to: DateConverter[D,T]) : D = {
    back.convert(to.convert(this).delete(elem))
  }


  def < = ???
  def <= = ???
  def > = ???
  def >= = ???
  def == = ???

  // the following has to be defined be the user
  def add(elem : DateElement[D]) : D
  def delete(elem : DateElement[D]) : D
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



