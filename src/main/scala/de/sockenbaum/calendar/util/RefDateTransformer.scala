package de.sockenbaum.calendar.util

import calendar.base._
import annotation.tailrec
import calendar.core._

/**
 * This is a util to create a calendar date to RefDate transformer in
 * very short time.
 * <p>
 * you have to define
 * <ul>
 * <li> all dates belonging to the date </li>
 * <li> a toRef Transformation </li>
 * </ul>
 * </p>
 * @author Ingolf Wagner <ingolf.wagner@zalando.de>
 */
trait RefDateTransformer[D <: Date[D]] extends DateTransformer[RefDate, D] {

  case class RefDateHelper[E <: DateElement[E]](op: DateOp[D, E], element: E)

  val zeroDate: D
  val toRef: DateTransformer[D, RefDate]

  // @todo : this must be optimized by step size analysis
  var elementList: List[RefDateHelper[_]] = Nil

  def ->[E <: DateElement[E]](element: E)(implicit op: DateOp[D, E]) {
    elementList = RefDateHelper(op, element) :: elementList
  }

  private def diff(a: D, b: D): BigInt = toRef.convert(a).millis - toRef.convert(b).millis

  /**
   * subfunction for iteratiron
   * @param incrementSeconds means decrement seconds
   * @param accuSeconds seconds accumulator
   * @param accuIterElements iterator accumulator
   * @param accuDate date accumulator
   * @return a date
   */
  @tailrec
  def step(incrementSeconds: Boolean,
           accuSeconds: BigInt,
           accuIterElements: List[RefDateHelper[_]],
           accuDate: D): D = {
    if (accuSeconds == 0) accuDate
    else {
      var nextIncrementSeconds = incrementSeconds
      var nextAccuSeconds = accuSeconds
      var nextAccuIterDate = accuIterElements
      var nextAccuDate = accuDate

      if (accuSeconds > 0) {
        if (incrementSeconds) {
          // switch
          nextIncrementSeconds = false
          nextAccuSeconds = accuSeconds
          nextAccuIterDate = accuIterElements.tail
          nextAccuDate = accuDate
        } else {
          // increment date
          // decrement seconds
          nextIncrementSeconds = false
          nextAccuIterDate = accuIterElements
          nextAccuDate = accuIterElements.head._1(accuDate)
          // seconds here are positive
          //nextAccuSeconds = accuSeconds - accuIterElements.head.toSecondsForAddition(accuDate)
          nextAccuSeconds = accuSeconds - accuDate.diff(nextAccuDate)
        }
      } else {
        if (incrementSeconds) {
          // increment seconds
          // decrement date
          nextIncrementSeconds = true
          nextAccuIterDate = accuIterElements
          nextAccuDate = accuIterElements.head._2(accuDate)
          // seconds here are negative
          //nextAccuSeconds = accuSeconds + accuIterElements.head.toSecondsForSubtraction(accuDate)
          nextAccuSeconds = accuSeconds + accuDate.diff(nextAccuDate)
        } else {
          nextIncrementSeconds = true
          nextAccuSeconds = accuSeconds
          nextAccuIterDate = accuIterElements.tail
          nextAccuDate = accuDate
        }
      }
      // just for debugging
      //println((nextAccuSeconds - accuSeconds) + " : " +  incrementSeconds + "," + accuSeconds + "," + accuIterElements + "," + accuDate)
      step(nextIncrementSeconds, nextAccuSeconds, nextAccuIterDate, nextAccuDate)
    }
  }


  def convert(r: RefDate): D = {
    val seconds = r.millis
    if (seconds == 0)
      zeroDate
    else if (seconds < 0)
      step(true, r.millis, iters, zeroDate)
    else
      step(false, r.millis, iters, zeroDate)
  }
}

