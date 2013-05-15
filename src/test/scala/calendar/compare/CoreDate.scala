package calendar.compare

import calendar.core._
import calendar.base.RefDate

/**
 * This Calendar type should represent a fully implemented Calendar class.
 *
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
  implicit val registerOne = new DateConnect[CoreDate, One]
  implicit val registerTen = new DateConnect[CoreDate, Ten]

  implicit val equal = new LazyCompare[CoreDate, CoreDate] {
    def equal(a: CoreDate, b: CoreDate): Boolean = a.i == b.i

    def less(a: CoreDate, b: CoreDate): Boolean = a.i < b.i
  }
  implicit val opOne = new DateOp[CoreDate, One] {
    def add(a: CoreDate, e: One): CoreDate = CoreDate(a.i + e.i)
  }
  implicit val opTen = new DateOp[CoreDate, Ten] {
    def add(a: CoreDate, e: Ten): CoreDate = CoreDate(a.i + 10 * e.i)
  }
  implicit val toRef = new DateTransformer[CoreDate, RefDate] {
    def convert(a: CoreDate): RefDate = RefDate(a.i)
  }

}
