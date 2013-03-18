package calendar.base


/**
 * date elements like Day, Month, Year
 */
abstract class DateElement[D: Date, E] {
  def add(date: D, elem: E): D

  def sub(date: D, elem: E): D

  def get(date: D): E
}
