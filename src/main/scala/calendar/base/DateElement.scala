package calendar.base

/**
 * date elements like Day, Month, Year
 */
trait DateElement[D <: Date[D]] {

  def calendar : Calendar[D]

  def transformedToMe[S <: Date[S]](date : S) : D =
    calendar.transformToMe(date)

  /**
   * correction function for subtraction
   * this function is meant to be overridden.
   * it returns the seconds of that are needed to
   * @param date
   * @return
   */
  def toSecondsForSubtraction[S <: Date[S]](date: S): BigInt = {
    val myDate = transformedToMe(date)
    myDate.toSeconds - (myDate - this).toSeconds
  }

  /**
   * correction function for subtraction
   * this function is meant to be overridden
   * @param date
   * @return
   */
  def toSecondsForAddition[S <: Date[S]](date: S): BigInt = {
    val myDate = transformedToMe(date)
    //println(date)
    (myDate + this).toSeconds - myDate.toSeconds
  }
}
