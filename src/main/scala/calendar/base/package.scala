package calendar

package object base {

  case class Millisecond(millis : BigInt)

  implicit object MillisecondElement extends DateElement[RefDate, Millisecond] {
    def add(date: RefDate, elem: Millisecond) = RefDate(Millisecond(date.millis.millis + elem.millis))
    def sub(date: RefDate, elem: Millisecond) = RefDate(Millisecond(date.millis.millis - elem.millis))
    def get(date: RefDate) = date.millis
  }
}
