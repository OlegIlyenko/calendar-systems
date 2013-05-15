package calendar.compare

import calendar.core.{DateTransformer, DateOp, DateElement, Date}
import calendar.base.RefDate

/**
 * a calendar system that is still under development. so i uses all that is there, without
 * defining optimized stuff
 *
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class TwoDimensionDate(ten: Int, rest: Int) extends Date[TwoDimensionDate]

object TwoDimensionDate {

  // --- operations

  implicit val opOne = new DateOp[TwoDimensionDate, One] {
    def add(a: TwoDimensionDate, e: One): TwoDimensionDate = TwoDimensionDate(a.ten + ((a.rest + e.i) / 10), (a.rest + e.i) % 10)
  }
  implicit val opTen = new DateOp[TwoDimensionDate, Ten] {
    def add(a: TwoDimensionDate, e: Ten): TwoDimensionDate = TwoDimensionDate(a.ten + e.i, a.rest)
  }

  // --- transformation

  implicit val toRef = new DateTransformer[TwoDimensionDate, RefDate] {
    def convert(a: TwoDimensionDate): RefDate = RefDate(10 * a.ten + a.rest)
  }
  implicit val fromRef = new DateTransformer[RefDate, TwoDimensionDate] {
    def convert(a: RefDate): TwoDimensionDate = TwoDimensionDate(a.millis.intValue() / 10, a.millis.intValue() % 10)
  }


}

