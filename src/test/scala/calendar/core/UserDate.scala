package calendar.core

import calendar.base._
import calendar.core._

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class UserDate(i: Int) extends Date[UserDate]

object UserDate {
  implicit val toRef = new DateConverter[UserDate, RefDate] {
    def convert(a: UserDate): RefDate = RefDate(a.i)
  }

  implicit val fromRef = new DateConverter[RefDate, UserDate] {
    def convert(a: RefDate): UserDate = UserDate(a.millis.intValue())
  }

}
