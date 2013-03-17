package calendar

import base.DateElement

/**
 * The Gregorian Calendar
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
package object gregorian {

  case class Second(s : BigInt) extends DateElement[GregDate]
  case class Minute(m : BigInt) extends DateElement[GregDate]
  case class Hour(h : BigInt) extends DateElement[GregDate]
  case class Day(d : BigInt) extends DateElement[GregDate]
  case class Month(m : BigInt) extends DateElement[GregDate]
  case class Year(y : BigInt) extends DateElement[GregDate]{
    def isLeap =
      if (y % 4 == 0)
        if (y % 100 == 0)
          y % 400 != 0
        else
          true
      else
        false
  }

}
