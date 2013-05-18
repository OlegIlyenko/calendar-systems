package de.sockenbaum.calendar.util

import calendar.base.Millisecond
import calendar.core.{DateOp, DateElement, Date}

/**
 * a test calendar that holds everything to the minute
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class MinuteDate(mili: Int, sec: Int, min: Int) extends Date[MinuteDate] {

}

case class Second(sec: Int) extends DateElement[Second] {
  def neg: Second = Second(-sec)
}

case class Minute(min: Int) extends DateElement[Minute] {
  def neg: Minute = Minute(-min)
}

object MinuteDate {

  implicit val addMillisecond = new DateOp[MinuteDate, Millisecond] {
    def add(a: MinuteDate, e: Millisecond): MinuteDate = {
      val milli = e.millis % 1000
      val secs = (e.millis / 1000) % 60
      val min = e.millis / 60000
      MinuteDate((a.min + min).intValue(), (a.sec + secs).intValue(), (a.mili + milli).intValue())
    }
  }
  implicit val addSecond = new DateOp[MinuteDate, Second] {
    def add(a: MinuteDate, e: Second): MinuteDate = {
      val sec = e.sec % 60
      val min = e.sec / 60
      MinuteDate(a.mili, a.sec + sec, a.min + min)
    }
  }
  implicit val addMinute = new DateOp[MinuteDate, Minute] {
    def add(a: MinuteDate, e: Minute): MinuteDate = MinuteDate(a.mili, a.sec, a.min + e.min)
  }
}
