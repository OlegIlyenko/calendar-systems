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
// @todo : this must be optimized by step size analysis
trait RefDateTransformer[D <: Date[D]] extends DateTransformer[RefDate, D] {

  case class RefDateHelper[E <: DateElement[E]](op: DateOp[D, E], element: E) {
    def add(date: D) = op.add(date, element)

    def sub(date: D) = op.add(date, element.neg)
  }

  val zeroDate: D
  val toRef: DateTransformer[D, RefDate]

  private var elementList: List[RefDateHelper[_]] = Nil

  // @todo : add implicit not found error message
  def ->[E <: DateElement[E]](element: E)(implicit op: DateOp[D, E]) {
    elementList = RefDateHelper(op, element) :: elementList
  }

  private def diff(a: D, b: D): BigInt = toRef.convert(a).millis - toRef.convert(b).millis

  def convert(r: RefDate): D = {
    val seconds = r.millis
    if (seconds == 0)
      zeroDate
    else if (seconds < 0)
      step(true, seconds, elementList, zeroDate)
    else
      step(false, seconds, elementList, zeroDate)
  }

  /**
   * sub function for iteration
   * @param incrementSeconds means decrement seconds
   * @param accuSeconds seconds accumulator
   * @param accuIterElements iterator accumulator
   * @param accuDate date accumulator
   * @return a date
   */
  @tailrec
  final def step(incrementSeconds: Boolean,
                 accuSeconds: BigInt,
                 accuIterElements: List[RefDateHelper[_]],
                 accuDate: D): D = {
    if (accuSeconds == 0) accuDate
    else {
      var nextIncrementSeconds = incrementSeconds
      var nextAccuSeconds = accuSeconds
      var nextAccuIterElements = accuIterElements
      var nextAccuDate = accuDate

      if (accuSeconds > 0) {
        if (incrementSeconds) {
          // switch
          nextIncrementSeconds = false
          nextAccuSeconds = accuSeconds
          nextAccuIterElements = accuIterElements.tail
          nextAccuDate = accuDate
        } else {
          // increment date
          // decrement seconds
          nextIncrementSeconds = false
          nextAccuIterElements = accuIterElements
          nextAccuDate = accuIterElements.head.add(accuDate)
          // seconds here are positive
          //nextAccuSeconds = accuSeconds - accuIterElements.head.toSecondsForAddition(accuDate)
          nextAccuSeconds = accuSeconds - diff(accuDate, nextAccuDate)
        }
      } else {
        if (incrementSeconds) {
          // increment seconds
          // decrement date
          nextIncrementSeconds = true
          nextAccuIterElements = accuIterElements
          nextAccuDate = accuIterElements.head.sub(accuDate)
          // seconds here are negative
          //nextAccuSeconds = accuSeconds + accuIterElements.head.toSecondsForSubtraction(accuDate)
          nextAccuSeconds = accuSeconds + diff(accuDate, nextAccuDate)
        } else {
          // switch
          nextIncrementSeconds = true
          nextAccuSeconds = accuSeconds
          nextAccuIterElements = accuIterElements.tail
          nextAccuDate = accuDate
        }
      }
      step(nextIncrementSeconds, nextAccuSeconds, nextAccuIterElements, nextAccuDate)
    }
  }


}

