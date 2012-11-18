package calendar.util

import calendar.base.{DateElement, Date}
import calendar.RefDate
import annotation.tailrec

/**
 * This is a util to make a Calendar in very short time.
 * You just have to define you Date format, the + and - function, and need a reference date
 *
 * <p>
 * the name is a bit misleading. The calendar is fast created, but is very slow in calculation
 * </p>
 * @author Ingolf Wagner <ingolf.wagner@zalando.de>
 * @param zeroDate the reference date for RefDate(0)
 */
class FastCalendarCreator[D <: Date[D]](zeroDate: D) {

  var iters: List[DateElement] = List()
  val zero = BigInt(0)

  /**
   * Iterater to step to the correct date
   * @param iterDate step size to create the date
   */
  def ->(iterDate: DateElement): FastCalendarCreator[D] = {
    iters = iterDate :: iters
    this
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
      def step(incrementSeconds: Boolean, accuSeconds: BigInt, accuIterDate: List[DateElement], accuDate: D): D = {
        // todo create a guard for endless loops
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
              nextAccuDate = accuDate + accuIterDate.head
              // seconds here are positive
              nextAccuSeconds = accuSeconds - accuIterDate.head.toSecondsForAddition(accuDate)
            }
          } else {
            if (incrementSeconds) {
              // increment seconds
              // decrement date
              nextIncrementSeconds = true
              nextAccuIterDate = accuIterDate
              nextAccuDate = accuDate - accuIterDate.head
              // seconds here are negative
              nextAccuSeconds = accuSeconds + accuIterDate.head.toSecondsForSubtraction(accuDate)
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

      val seconds = r.seconds
      // just for debugging
      // println("start " + r.seconds)
      if (seconds == 0)
        zeroDate
      else if (seconds < 0)
        step(true, r.seconds, iters, zeroDate)
      else
        step(false, r.seconds, iters, zeroDate)
    }
  }
}
