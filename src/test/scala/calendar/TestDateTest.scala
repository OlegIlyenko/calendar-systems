package calendar


import dummy._
import dummy.UserDate._
import dummy.InterDate._
import dummy.TestDate._


import org.scalatest.FlatSpec

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
class TestDateTest extends FlatSpec {

  "Operation with own DateElements" should "find correct Operator" in {
    val date = TestDate(0, 0)
    assert((date + Foo(1)) == TestDate(1, 0))
    assert((date + Bar(1)) == TestDate(0, 1))
  }

  "Operation with other DateElements" should "find correct Operator" in {
    val date = InterDate(0)
    assert(date + Foo(1) == InterDate(1))
    assert(date + Bar(1) == InterDate(-1))
  }

  "UserDates" should "find correct Operator" in {
    val date = UserDate(0)
    assert(date + Foo(1) == UserDate(1))
    assert(date + Bar(1) == UserDate(0))
  }

}
