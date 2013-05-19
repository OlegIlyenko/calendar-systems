package de.sockenbaum.calendar.util

import calendar.compare._
import calendar.compare.CoreDate._
import calendar.compare.ShiftTenDate._
import calendar.base._
import calendar.core._

import MinuteDate._

import org.scalatest.FlatSpec


/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
class RefDateTransformTest extends FlatSpec {

  "Transformation" should "work" in {
    assert(ShiftTenDate(1) - Ten(1) === CoreDate(1))
    //    assert(MinuteDate(0, 0, 0) + One(1) === CoreDate(1))
    //    assert(MinuteDate(1, 0, 0) + One(1) === MinuteDate(1, 0, 1))
    //    assert(MinuteDate(0, 0, 0) + Second(30) === MinuteDate(0, 30, 0), "normal algebra")
    //    assert(CoreDate(0) + Second(30) === MinuteDate(0, 30, 0), "addition not working")
    //    assert(CoreDate(0) + Minute(2) + Second(30) === MinuteDate(2, 30, 0), "addition 2 not working")
    //    assert(CoreDate(0) + Minute(2) + Second(30) !== MinuteDate(3, 30, 0), "addition 3 not working")

  }

}
