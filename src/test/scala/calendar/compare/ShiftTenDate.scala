package calendar.compare

import calendar.core.{DateTransformer, DateCompare, DateOp}
import calendar.base.RefDate

/**
 * Like the Core Date but a bit less implemented and Shifted Zero
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class ShiftTenDate(i: Int)

object ShiftTenDate {

  // --- operation

  implicit val opOne = new DateOp[ShiftTenDate, One] {
    def add(a: ShiftTenDate, e: One): ShiftTenDate = ShiftTenDate(a.i + e.i)
  }
  implicit val opTen = new DateOp[ShiftTenDate, Ten] {
    def add(a: ShiftTenDate, e: Ten): ShiftTenDate = ShiftTenDate(a.i + 10 * e.i)
  }

  // -- transformation

  implicit val toRef = new DateTransformer[ShiftTenDate, RefDate] {
    def convert(a: ShiftTenDate): RefDate = RefDate(10 + a.i)
  }

  implicit val fromRef = new DateTransformer[RefDate, ShiftTenDate] {
    def convert(a: RefDate): ShiftTenDate = ShiftTenDate(a.millis.intValue() - 10)
  }
}
