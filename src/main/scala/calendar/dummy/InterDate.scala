package calendar.dummy

import calendar.core._
import calendar.base._

/**
 * A Date to operate with not the same Date System
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class InterDate(i: Int) extends Date[InterDate]

object InterDate {
  implicit val interOpFoo = new DateOp[InterDate, Foo] {
    def add(a: InterDate, e: Foo): InterDate = InterDate(a.i + e.i)
  }
  implicit val interOpBar = new DateOp[InterDate, Bar] {
    def add(a: InterDate, e: Bar): InterDate = InterDate(a.i - e.i)
  }
}
