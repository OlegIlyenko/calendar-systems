package calendar.compare

import calendar.core._
import calendar.compare.Ten
import calendar.compare.One

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class CoreDate(i: Int) extends Date[CoreDate]

case class One(i: Int) extends DateElement[One] {
  def neg: One = One(-i)
}

case class Ten(i: Int) extends DateElement[Ten] {
  def neg: Ten = Ten(-i)
}

object CoreDate {
  implicit val equal = new LazyCompare[CoreDate, CoreDate] {
    def equal(a: CoreDate, b: CoreDate): Boolean = a.i == b.i

    def less(a: CoreDate, b: CoreDate): Boolean = a.i < b.i
  }
  implicit val coreOpOne = new DateOp[CoreDate, One] {
    def add(a: CoreDate, e: One): CoreDate = CoreDate(a.i + e.i)
  }
  implicit val coreOpTen = new DateOp[CoreDate, Ten] {
    def add(a: CoreDate, e: Ten): CoreDate = CoreDate(a.i + 10 * e.i)
  }


}
