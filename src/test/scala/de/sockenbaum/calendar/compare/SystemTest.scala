package calendar.compare

import org.scalatest.FlatSpec

import CoreDate._
import FuckUpDate._
import ShiftTenDate._
import TwoDimensionDate._
import calendar.base.RefDate


/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
class SystemTest extends FlatSpec {

  "DateCompare" should "find definitions" in {
    assert(CoreDate(100) === CoreDate(100))
    assert(CoreDate(101) !== CoreDate(100))
    assert(CoreDate(100) === RefDate(100))
  }
  it should "find fallback definitions" in {
    assert(RefDate(100) === CoreDate(100))
    assert(CoreDate(100) === ShiftTenDate(90))
    assert(ShiftTenDate(100) === CoreDate(110))
    assert(ShiftTenDate(90) === RefDate(100))
  }
  it should "be found by all relations" in {
    assert(CoreDate(30) <= ShiftTenDate(199))
    assert(ShiftTenDate(100) >= CoreDate(30))
    assert(CoreDate(30) < ShiftTenDate(199))
    assert(ShiftTenDate(100) > CoreDate(30))
  }
  it should "also work" in {
    assert(FuckUpDate(1) === CoreDate(1))
    assert(FuckUpDate(1) === ShiftTenDate(1))
    assert(ShiftTenDate(1) !== CoreDate(1))
  }

  "DateOp" should "find definition" in {
    assert(CoreDate(10) + One(1) === CoreDate(11))
    assert(CoreDate(10) + Ten(1) === CoreDate(20))
    assert(CoreDate(20) === CoreDate(10) + Ten(1))
    assert(CoreDate(11) === CoreDate(10) + One(1))
  }
  it should "find fallback definitions" in {
    assert(ShiftTenDate(1) + Ten(1) === CoreDate(21))
    assert(ShiftTenDate(1) - Ten(1) === CoreDate(1))
    assert(CoreDate(10) - Ten(1) < TwoDimensionDate(1, 1))
  }


}
