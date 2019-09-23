package cs320

import cs320._

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = ???
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def run(str: String): List[Int] = ???

  def tests: Unit = {
    test(run("{+ 3 7}"), List(10))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{min 3 4 5}"), List(3))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
  }
}
