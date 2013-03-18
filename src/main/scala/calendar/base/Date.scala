package calendar.base

import calendar.util.FlowHelper._
import Date._
import calendar.gregorian.GregDate
import GregDate._

/**
 * The Date Type Class
 *
 * No sugar is defined here :)
 */
trait Date[D] {
  def add[E](date: D, elem: E)(implicit elemEv: DateElement[D, E]): D =
    elemEv.add(date, elem)
  def sub[E](date: D, elem: E)(implicit elemEv: DateElement[D, E]): D =
    elemEv.add(date, elem)
}

class DateOps[D : Date](me: D) {
  private type CurrDateElement[E] = DateElement[D, E]

  def toCalendar[A](implicit conv: Convert[D, A]) = conv convert me

  def add[E: CurrDateElement](elem: E) = implicitly[Date[D]].add(me, elem)

  def sub[E: CurrDateElement](elem: E) = implicitly[Date[D]].sub(me, elem)

  def get[E: CurrDateElement](elem: E) = implicitly[CurrDateElement[E]].get(me)

  def +[T, E](elem: E)(implicit dateEv: Date[T], elemEv: DateElement[T, E], back: Convert[T, D], to: Convert[D, T]): D =
    me |> to.convert |> (_ add elem) |> back.convert


  def -[T, E](elem: E)(implicit dateEv: Date[T], elemEv: DateElement[T, E], back: Convert[T, D], to: Convert[D, T]): D =
    me |> to.convert |> (_ sub elem) |> back.convert

  def <[T](date: T)(implicit meToRef: Convert[D, RefDate], itToRef: Convert[T, RefDate]) =
    me |> meToRef.convert |> (_ smaller (date |> itToRef.convert))

  def >[T](date: T)(implicit meToRef: Convert[D, RefDate], itToRef: Convert[T, RefDate]) =
    me |> meToRef.convert |> (_ greater (date |> itToRef.convert))

  def <=[T](date: T)(implicit ev: Date[T], meToRef: Convert[D, RefDate], itToRef: Convert[T, RefDate]) =
    (me |> meToRef.convert, date |> itToRef.convert) |> {case (a, b) => (a smaller b) || (a equal b)}

  def >=[T](date: T)(implicit ev: Date[T], meToRef: Convert[D, RefDate], itToRef: Convert[T, RefDate]) =
    (me |> meToRef.convert, date |> itToRef.convert) |> {case (a, b) => (a greater b) || (a equal b)}

  def ==[T](date: T)(implicit meToRef: Convert[D, RefDate], itToRef: Convert[T, RefDate]) =
    me |> meToRef.convert |> (_ equal (date |> itToRef.convert))

  /**
   * @return seconds that differ between the other date
   */
  def diff[T](date : T)(implicit ev: Date[T], meToRef: Convert[D, RefDate], itToRef : Convert[T, RefDate]) =
    meToRef.convert(me).millis.millis - itToRef.convert(date).millis.millis
}

/**
 * default converters are defined here.
 *
 * the converter magic is also happening here.
 */
object Date extends bbb {
  implicit def identityConvert[T] = new Convert[T, T] {
    def convert(a: T) = a
  }

  implicit def dateSugar[D : Date](date: D): DateOps[D] = new DateOps(date)

  type ToRefDateConvert[T] = Convert[T, RefDate]
  type FromRefDateConvert[T] = Convert[RefDate, T]

  implicit object GregToRef extends Convert[GregDate, RefDate] {
    def convert(a: GregDate) = RefDate(Millisecond(
      yearToMillisecs(a) +
        monthToMillisecs(a.month, isLeapYear(a.year.y)) +
        oneDay * (a.day.d - 1) +
        oneHour * a.hour.h +
        oneMinute * a.min.m +
        oneSecond * a.sec.s +
        a.mil.millis
    ))
  }
}

trait bbb extends fff{
  implicit object RefToGreg extends Convert[RefDate, GregDate] {
    def convert(a: RefDate) = refHelper(a)
  }
}

trait fff {
  implicit def standardDateConvert[A: ToRefDateConvert, B : FromRefDateConvert] =
    Convert[A, B](_ |> implicitly[ToRefDateConvert[A]].convert |> implicitly[FromRefDateConvert[B]].convert)
}


