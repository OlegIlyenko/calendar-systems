package de.sockenbaum.calendar.util

import org.scalatest.FlatSpec

import calendar.compare._

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
class RefDateTransformTest extends FlatSpec {

  "Transformation" should "work" in {
    assert(ShiftTenDate(1) - Ten(1) === CoreDate(1))

    assert(MinuteDate(0, 0, 0) + One(1) === CoreDate(1))
    assert(MinuteDate(1, 0, 0) + One(1) === MinuteDate(1, 0, 1))
    assert(MinuteDate(0, 0, 0) + Second(30) === MinuteDate(0, 30, 0), "normal algebra")

    assert(CoreDate(0) + Second(30) === MinuteDate(0, 30, 0), "addition not working")
    assert(CoreDate(0) + Minute(2) + Second(30) === MinuteDate(2, 30, 0), "addition 2 not working")
    assert(CoreDate(0) + Minute(2) + Second(30) !== MinuteDate(3, 30, 0), "addition 3 not working")
  }

  it should " can be overriden by user in order to have different performace/precision characteristics" in {
    import  FastButImpreciseOneAndTenOperations._

    assert(ShiftTenDate(1) - Ten(1) === CoreDate(1))
  }

}
