package calendar

import base.DateElement
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * @author Ingolf Wagner <ingolf.wagner@zalando.de>
 */
class GregorianCalendarSuite extends FlatSpec with ShouldMatchers{

  val month31:BigInt = 2678400
  val month30:BigInt = 2592000
  val month29:BigInt = 2505600
  val month28:BigInt = 2419200


  "For Gregorian DateElements" should "leap less years transform correct to seconds" in {
    val year1 = Year(1970)
    val year2 = Year(1971)
    assert(year1.toRefSeconds == 0, year1 + " != 0")
    assert(year2.toRefSeconds == 31536000 , year2 + " != " + 31536000)
  }

  it should "leap years transform correct to seconds" in {
    val yearSeconds = 31536000 + 31536000 + 31622400
    val year1 = Year(1972)
    assert(year1.isLeap, "1972 is not leap year")
    val year2 = Year(1973)
    assert(year2.toRefSeconds == yearSeconds, year2.toRefSeconds + " != " + yearSeconds)
  }

  it should "count correct number of leap years " in {
    assert(GregorianCalendar.numberOfLeapYears(1970, 1973) == 1, "73")
    assert(GregorianCalendar.numberOfLeapYears(1970, 1972) == 1, "72")
    assert(GregorianCalendar.numberOfLeapYears(1970, 1971) == 0, "71")
    assert(GregorianCalendar.numberOfLeapYears(1970, 1968) == 1, "68")
    assert(GregorianCalendar.numberOfLeapYears(1972, 1968) == 2, "68-72")
    assert(GregorianCalendar.numberOfLeapYears(1999, 2001) == 0, "2000")
  }

  def checkDateElementSubtraction(toSubtractFromDate : GregorianDate,
                                  elementToSubtract : GregorianDateElement,
                                  resultingSeconds : BigInt) {
    val subtraction : BigInt = elementToSubtract.toSecondsForSubtraction(toSubtractFromDate)
    assert(subtraction == resultingSeconds,
      elementToSubtract + " - " + toSubtractFromDate + " = " + (toSubtractFromDate - elementToSubtract) +
        " : " +  subtraction + " != " + resultingSeconds)
  }

  def checkDateElementAddition(toAddToDate : GregorianDate,
                               elementToAdd : GregorianDateElement,
                               resultingSeconds : BigInt) {
    val addition : BigInt = elementToAdd.toSecondsForAddition(toAddToDate)
    assert(addition == resultingSeconds,
      elementToAdd + " + " + toAddToDate + " = " + (toAddToDate + elementToAdd) +
        " : " +  addition + " != " + resultingSeconds)
  }


  it should "month in leap less years transform correct to seconds for subtraction" in {
    checkDateElementSubtraction(GregorianCalendar.create(1971,12,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1971,11,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1971,10,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1971,9 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1971,8 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1971,7 ,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1971,6 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1971,5 ,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1971,4 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1971,3 ,1,0,0,0), Month(1), month28)
    checkDateElementSubtraction(GregorianCalendar.create(1971,2 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1971,1 ,1,0,0,0), Month(1), month31)
  }

  it should "month in leap less years transform correct to seconds for addition" in {
    checkDateElementAddition(GregorianCalendar.create(1971,12,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1971,11,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1971,10,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1971,9 ,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1971,8 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1971,7 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1971,6 ,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1971,5 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1971,4 ,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1971,3 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1971,2 ,1,0,0,0), Month(1), month28)
    checkDateElementAddition(GregorianCalendar.create(1971,1 ,1,0,0,0), Month(1), month31)
  }

  it should "month in leap years transform correct to seconds for subtraction" in {
    checkDateElementSubtraction(GregorianCalendar.create(1972,12,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1972,11,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1972,10,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1972,9 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1972,8 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1972,7 ,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1972,6 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1972,5 ,1,0,0,0), Month(1), month30)
    checkDateElementSubtraction(GregorianCalendar.create(1972,4 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1972,3 ,1,0,0,0), Month(1), month29)
    checkDateElementSubtraction(GregorianCalendar.create(1972,2 ,1,0,0,0), Month(1), month31)
    checkDateElementSubtraction(GregorianCalendar.create(1972,1 ,1,0,0,0), Month(1), month31)
  }

  it should "month in leap years transform correct to seconds for addition" in {
    checkDateElementAddition(GregorianCalendar.create(1972,12,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1972,11,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1972,10,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1972,9 ,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1972,8 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1972,7 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1972,6 ,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1972,5 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1972,4 ,1,0,0,0), Month(1), month30)
    checkDateElementAddition(GregorianCalendar.create(1972,3 ,1,0,0,0), Month(1), month31)
    checkDateElementAddition(GregorianCalendar.create(1972,2 ,1,0,0,0), Month(1), month29)
    checkDateElementAddition(GregorianCalendar.create(1972,1 ,1,0,0,0), Month(1), month31)
  }


  def checkDate(date : GregorianDate) {
    lazy val ref = date.toRef
    try{
      ref
    }catch {
      case _: Throwable => assert(condition = false, clue = "could not transform to refDate " + date)
    }
    lazy val fromRef = GregorianCalendar.fromRef(ref)
    try{
      fromRef
    } catch {
      case _: Throwable => assert(condition = false, clue = "could not transform to Gregorian Calendar " + ref)
    }

    try {
      assert(date == fromRef, "" + date + " != " + fromRef + " => " + date.toSeconds + " != " + fromRef.toSeconds)
    } catch {
      case _: Throwable => assert(condition = false, clue = "could not equal dates")
    }
  }

  "For Dates " should "equal be correct" in  {
    val date1 = GregorianCalendar.create(2000, 6, 5, 3, 2, 1)
    val date2 = GregorianCalendar.create(2000, 6, 5, 3, 2, 1)
    assert(date1 == date2)
    assert(date1.toRef == date2)
    assert(date1 == date2.toRef)
  }


  it should "retransform correct with year precission" in {
    checkDate(GregorianCalendar.create(1970, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(1971, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(1972, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(1973, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(1974, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(1975, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2000, 1, 1, 0, 0, 0))
  }

  it should "retransform correct with month precission" in  {
    checkDate(GregorianCalendar.create(2001, 1, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 2, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 3, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 4, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 5, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 6, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 7, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 8, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 9, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 10, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 11, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2001, 12, 1, 0, 0, 0))
  }

  it should "retransform correct with day precission" in  {
    checkDate(GregorianCalendar.create(2000, 3, 3, 0, 0, 0))
    checkDate(GregorianCalendar.create(2000, 3, 1, 0, 0, 0))
    checkDate(GregorianCalendar.create(2000, 2, 28, 0, 0, 0))
    checkDate(GregorianCalendar.create(2000, 12, 31, 0, 0, 0))
    checkDate(GregorianCalendar.create(2000, 4, 6, 0, 0, 0))
    checkDate(GregorianCalendar.create(2000, 5, 18, 0, 0, 0))
  }

  it should "retransform correct with hour precission" in  {
    checkDate(GregorianCalendar.create(2000, 4, 3, 1, 0, 0))
  }

  it should "retransform correct with minute precission" in  {
    checkDate(GregorianCalendar.create(2000, 5, 4, 2, 1, 0))
  }

  it should "retransform correct with second precission" in   {
    checkDate(GregorianCalendar.create(2000, 6, 5, 3, 2, 1))
  }

  it should "add correct" in  {
    val date1 = GregorianCalendar.create(2000, 6, 6, 3, 2, 1)
    val date2 = GregorianCalendar.create(2000, 6, 5, 3, 2, 1)
    assert(date1 == date2 + Day(1), "addition does not work")
    assert(date1 == (date2 + Day(1)).toRef, "addtion does not work")
    assert(date1.toRef == (date2 + Day(1)).toRef, "addtion does not work with refcal transformation")
    assert(date1.toRef == date2 + Day(1), "addtion does not work in relation to refcal")
    // assert(date1.toRef == (date2.toRef + Day(1)), "refcal addtion does not work")
  }

  it should "substract correct" ignore {
    // todo write me
  }
}
