package de.sockenbaum.calendar.util

import calendar.compare.CoreDate._
import calendar.compare._
import calendar.base._
import calendar.core._
import org.scalatest.FlatSpec

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
class RefDateTransformTest extends FlatSpec {

  "Transformation" should "work" in {
    assert(MinuteDate(0, 0, 0) + One(1) === CoreDate(1))
  }

}
