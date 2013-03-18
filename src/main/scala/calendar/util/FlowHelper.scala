package calendar.util

/**
 * Some useful stuff
 * 
 * @author Oleg Ilyenko
 */
object FlowHelper {
  implicit def toAnyOps[T](t: T) = new {
    def |>[R](fn: T => R) = fn(t)
    def <|[R](fn: T => Unit) = {
      fn(t)
      t
    }
  }
}
