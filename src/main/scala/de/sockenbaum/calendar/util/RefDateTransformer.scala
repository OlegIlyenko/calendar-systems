package de.sockenbaum.calendar.util

import calendar.base._
import annotation.tailrec
import calendar.core.{DateOp, DateConnect, DateElement, Date}

/**
 * This is a util to make a Date in very short time.
 * You just have to define you Date format, the + and - function, and need a reference date
 *
 * <p>
 * the name is a bit misleading. The calendar is fast created, but is very slow in calculation
 * </p>
 * @author Ingolf Wagner <ingolf.wagner@zalando.de>
 */
abstract class RefDateTransformer[D <: Date[D]] {

  case class RefDateHelper[E <: DateElement[E]](op: DateOp[D, E], element: E)

  def zeroDate: D

  var list: List[RefDateHelper[_]] = Nil

  /*
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
  } */
}

