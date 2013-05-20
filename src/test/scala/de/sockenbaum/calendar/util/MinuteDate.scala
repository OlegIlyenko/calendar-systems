package de.sockenbaum.calendar.util

import calendar.base._
import calendar.core._

/**
 * a test calendar that holds everything to the minute
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class MinuteDate(min: Int, sec: Int, mili: Int) extends Date[MinuteDate] {

}

case class Second(sec: Int) extends DateElement[Second] {
  def neg: Second = Second(-sec)
}

object Second {
  implicit object PreferredOperatorForSecond extends PreferredOperator[MinuteDate, Second]
}

case class Minute(min: Int) extends DateElement[Minute] {
  def neg: Minute = Minute(-min)
}

object Minute {
  implicit object PreferredOperatorForMinute extends PreferredOperator[MinuteDate, Minute]
}

object MinuteDate {

  // --- register date elements
  implicit val registerMinute = new DateConnect[MinuteDate, Minute]
  implicit val registerSecond = new DateConnect[MinuteDate, Second]

  // --- operator definitions
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
      MinuteDate(a.min + min, a.sec + sec, a.mili)
    }
  }
  implicit val addMinute = new DateOp[MinuteDate, Minute] {
    def add(a: MinuteDate, e: Minute): MinuteDate = MinuteDate(a.min + e.min, a.sec, a.mili)
  }

  // --- transformations
  implicit val toRef = new DateTransformer[MinuteDate, RefDate] {
    def convert(a: MinuteDate): RefDate = RefDate(a.min * 60000 + a.sec * 1000 + a.mili)
  }
  implicit val fromRef = new RefDateTransformer[MinuteDate] {
    val zeroDate: MinuteDate = MinuteDate(0, 0, 0)
    val toRef: DateTransformer[MinuteDate, RefDate] = MinuteDate.toRef
  } -> Millisecond(1) -> Second(1) -> Minute(1)
}
