package calendar.gregorian

import calendar.base._
import calendar.base.Millisecond

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class GregDate(year : Year, month : Month, day : Day, hour : Hour, min : Minute, sec : Second, mil : Millisecond)
  extends Date {
  type D = GregDate

  def add(elem: DateElement[GregDate#D]) = null

  def delete(elem: DateElement[GregDate#D]) = null
}


object GregDate {


  val leapYear: BigInt     = 31622400
  val leapLessYear: BigInt = 31536000
  val month31: BigInt      = 2678400
  val month30: BigInt      = 2592000
  val month29: BigInt      = 2505600
  val month28: BigInt      = 2419200
  val oneDay: BigInt       = 86400
  val oneHour: BigInt      = 3600
  val oneMinute: BigInt    = 60
  val oneSecond: BigInt    = 1


  def create(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int) =
    GregDate(Year(year), Month(month), Day(day), Hour(hour), Minute(minute), Second(second), Millisecond(0))

  def toRef = new DateConverter[GregDate, RefDate] {
    def convert(a: GregDate) =
  }
}
