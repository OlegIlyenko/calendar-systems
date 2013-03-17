package calendar

import base.DateElement

/**
 * The Gregorian Calendar
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
package object gregorian {

  case class Second(s : Int) extends DateElement[GregDate]
  case class Minute(m : Int) extends DateElement[GregDate]
  case class Hour(h : Int) extends DateElement[GregDate]
  case class Day(d : Int) extends DateElement[GregDate]
  case class Month(m : Int) extends DateElement[GregDate]{
    def daysOfMonth(isLeap: Boolean): Int = m % 12 match {
      case 1 => 31
      case 2 => if (isLeap) 29 else 28
      case 3 => 31
      case 4 => 30
      case 5 => 31
      case 6 => 30
      case 7 => 31
      case 8 => 31
      case 9 => 30
      case 10 => 31
      case 11 => 30
      case 0 => 31
    }
  }
  case class Year(y : Int) extends DateElement[GregDate]
}
