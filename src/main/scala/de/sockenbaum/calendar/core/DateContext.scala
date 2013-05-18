package de.sockenbaum.calendar.core

import calendar.core.{DateElement, Date}

/**
 * a Filter is for evaluating if a DatElement is
 * contained in the actual calendar representation.
 *
 * this makes it possible to create context depending formulations, like
 * is it Friday
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
abstract class DateContext[D <: Date[D], E <: DateElement[E]] {
  def contains(date: D, element: E): Boolean
}
