package calendar.compare

import calendar.core.{DateTransformer, DateCompare, DateOp}
import calendar.base.RefDate

/**
 * Like the Core Date but a bit less implemented and Shifted Zero
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class ShiftTenDate(i: Int)

object ShiftTenDate {

  // -- transformation

  implicit val toRef = new DateTransformer[ShiftTenDate, RefDate] {
    def convert(a: ShiftTenDate): RefDate = RefDate(10 + a.i)
  }

  implicit val fromRef = new DateTransformer[RefDate, ShiftTenDate] {
    def convert(a: RefDate): ShiftTenDate = ShiftTenDate(a.millis.intValue() - 10)
  }
}
