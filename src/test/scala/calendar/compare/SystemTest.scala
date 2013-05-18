package calendar.compare

import org.scalatest.FlatSpec

import CoreDate._
import FuckUpDate._
import ShiftTenDate._
import TwoDimensionDate._


import calendar.base.RefDate._
import calendar.base.RefDate


/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
class SystemTest extends FlatSpec {

  "System" should "find correct Equals object" in {
    assert(CoreDate(100) === CoreDate(100))
    assert(CoreDate(101) !== CoreDate(100))
    assert(CoreDate(100) === RefDate(100))
    assert(CoreDate(100) === RefDate(100))
    assert(CoreDate(100) === ShiftTenDate(90))
    assert(ShiftTenDate(100) === CoreDate(110))
  }

}
