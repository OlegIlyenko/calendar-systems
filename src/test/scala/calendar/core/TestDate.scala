package calendar.core

import calendar.base._
import calendar.core._

/**
 * @author Ingolf Wagner <palipalo9@googlemail.com>
 */
case class TestDate(foo: Int, bar: Int) extends Date[TestDate]

case class Foo(i: Int) extends DateElement[Foo] {
  def neg: Foo = Foo(-i)
}

case class Bar(i: Int) extends DateElement[Bar] {
  def neg: Bar = Bar(-i)
}


object TestDate {
  implicit val addFoo = new DateOp[TestDate, Foo] {
    def add(a: TestDate, e: Foo): TestDate = TestDate(a.foo + e.i, a.bar)
  }
  implicit val addBar = new DateOp[TestDate, Bar] {
    def add(a: TestDate, e: Bar): TestDate = TestDate(a.foo, a.bar + e.i)
  }

  // --------------- converters

  implicit val toRef = new DateConverter[TestDate, RefDate] {
    def convert(a: TestDate): RefDate = RefDate(a.foo)
  }
  implicit val fromRef = new DateConverter[RefDate, TestDate] {
    def convert(a: RefDate): TestDate = TestDate(a.millis.intValue(), 0)
  }

  // ---------------- register DateElements
  implicit val foo = new DateConnect[TestDate, Foo]
  implicit val bar = new DateConnect[TestDate, Bar]
}

