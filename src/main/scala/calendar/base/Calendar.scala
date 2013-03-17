package calendar.base

/**
 * The Calendar interface
 *
 * Here all the sugar is defined
 */
trait Calendar {
  type C <: Calendar

  def toCalendar[A](implicit evidence: CalendarConverter[C, A]) =
    evidence.convert(this.asInstanceOf[C])

  // todo def + -

}


/**
 * default converters are defined here.
 *
 * the converter magic is also happening here.
 */
object Calendar {
  implicit def identityConverter[T <: Calendar] = new CalendarConverter[T, T] {
    def convert(a: T) = a
  }

  implicit def standardConverter[A <: Calendar, B <: Calendar]
  (implicit ev1: CalendarConverter[A, RefCalendar], ev2: CalendarConverter[RefCalendar, B]) = new CalendarConverter[A, B] {
    def convert(a: A) = ev2.convert(ev1.convert(a))
  }

}



