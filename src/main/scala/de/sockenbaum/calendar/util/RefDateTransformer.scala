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

  /**
   * creates the fromRef date
   * @return
   */
  def finished: RefDate => D = {
    r: RefDate => {
      /**
       * subfunction for iteratiron
       * @param incrementSeconds means decrement seconds
       * @param accuSeconds seconds accumulator
       * @param accuIterDate iterator accumulator
       * @param accuDate date accumulator
       * @return a date
       */
      @tailrec
      def step(incrementSeconds: Boolean, accuSeconds: BigInt, accuIterDate: List[AddSubDate], accuDate: D): D = {
        if (accuSeconds == 0) accuDate
        else {
          var nextIncrementSeconds = incrementSeconds
          var nextAccuSeconds = accuSeconds
          var nextAccuIterDate = accuIterDate
          var nextAccuDate = accuDate

          if (accuSeconds > 0) {
            if (incrementSeconds) {
              // switch
              nextIncrementSeconds = false
              nextAccuSeconds = accuSeconds
              nextAccuIterDate = accuIterDate.tail
              nextAccuDate = accuDate
            } else {
              // increment date
              // decrement seconds
              nextIncrementSeconds = false
              nextAccuIterDate = accuIterDate
              nextAccuDate = accuIterDate.head._1(accuDate)
              // seconds here are positive
              //nextAccuSeconds = accuSeconds - accuIterDate.head.toSecondsForAddition(accuDate)
              nextAccuSeconds = accuSeconds - accuDate.diff(nextAccuDate)
            }
          } else {
            if (incrementSeconds) {
              // increment seconds
              // decrement date
              nextIncrementSeconds = true
              nextAccuIterDate = accuIterDate
              nextAccuDate = accuIterDate.head._2(accuDate)
              // seconds here are negative
              //nextAccuSeconds = accuSeconds + accuIterDate.head.toSecondsForSubtraction(accuDate)
              nextAccuSeconds = accuSeconds + accuDate.diff(nextAccuDate)
            } else {
              nextIncrementSeconds = true
              nextAccuSeconds = accuSeconds
              nextAccuIterDate = accuIterDate.tail
              nextAccuDate = accuDate
            }
          }
          // just for debugging
          //println((nextAccuSeconds - accuSeconds) + " : " +  incrementSeconds + "," + accuSeconds + "," + accuIterDate + "," + accuDate)
          step(nextIncrementSeconds, nextAccuSeconds, nextAccuIterDate, nextAccuDate)
        }
      }

      val seconds = r.millis.millis
      // just for debugging
      // println("start " + r.seconds)
      if (seconds == 0)
        zeroDate
      else if (seconds < 0)
        step(true, r.millis.millis, iters, zeroDate)
      else
        step(false, r.millis.millis, iters, zeroDate)
    }
  }

  def convert(a: RefDate): D = null
}

