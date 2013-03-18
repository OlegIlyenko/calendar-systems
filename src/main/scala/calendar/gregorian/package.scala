package calendar

import base.DateElement

/**
 * The Gregorian Calendar
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
package object gregorian {

  case class Second(s : Int)

  object Second {
    implicit object SecondElement extends DateElement[GregDate, Second] {
      def add(date: GregDate, elem: Second) = date addSecond elem.s
      def sub(date: GregDate, elem: Second) = date subSecond elem.s
      def get(date: GregDate) = date.sec
    }
  }

  case class Minute(m : Int)

  object Minute {
    implicit object MinuteElement extends DateElement[GregDate, Minute] {
      def add(date: GregDate, elem: Minute) = date addMinute elem.m
      def sub(date: GregDate, elem: Minute) = date subMinute  elem.m
      def get(date: GregDate) = date.min
    }
  }

  case class Hour(h : Int)

  object Hour {
    implicit object HourElement extends DateElement[GregDate, Hour] {
      def add(date: GregDate, elem: Hour) = date addHour elem.h
      def sub(date: GregDate, elem: Hour) = date subHour elem.h
      def get(date: GregDate) = date.hour
    }
  }

  case class Day(d : Int)

  object Day {
    implicit object DayElement extends DateElement[GregDate, Day] {
      def add(date: GregDate, elem: Day) = date addDay elem.d
      def sub(date: GregDate, elem: Day) = date subDay elem.d
      def get(date: GregDate) = date.day
    }
  }

  case class Month(m : Int) {
    def daysOfMonth(isLeap: Boolean): Int = m % 12 match {
      case 1 => 31
      case 2 => if (isLeap) 29 else 28
      case 3 => 31
      case 4 => 30
      case 5 => 31
      case 6 => 30
      case 7 => 31
      case 8 => 31
      case 9 => 30
      case 10 => 31
      case 11 => 30
      case 0 => 31
    }
  }

  object Month {
    implicit object MonthElement extends DateElement[GregDate, Month] {
      def add(date: GregDate, elem: Month) = date addMonth elem.m
      def sub(date: GregDate, elem: Month) = date subMonth elem.m
      def get(date: GregDate) = date.month
    }
  }

  case class Year(y : Int) {
    val leapYear: BigInt = 31622400 // 366 Days
    val leapLessYear: BigInt = 31536000 // 365 Days

    def toRefSeconds = {
      val years = y - 1970
      val leapYears =
        if (years > 0) GregDate.numberOfLeapYears(1970, y - 1)
        else GregDate.numberOfLeapYears(y, 1970 - 1)

      leapLessYear * (years - leapYears) + leapYear * leapYears
    }

    def isLeap = GregDate.isLeapYear(y)
  }

  object Year {
    implicit object YearElement extends DateElement[GregDate, Year] {
      def add(date: GregDate, elem: Year) = date addYear elem.y
      def sub(date: GregDate, elem: Year) = date subYear elem.y
      def get(date: GregDate) = date.year
    }
  }
}
