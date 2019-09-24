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
      def f(r: Int): Int = op(l, r)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  // Interpreter for MUWAE
  def run(str: String): List[Int] = {
    // (Int, Int) => Int functions for the interpreter
    def add(l: Int, r: Int): Int = l + r
    def sub(l: Int, r: Int): Int = l - r
    def max(l: Int, r: Int): Int = if (l < r) r else l
    def min(l: Int, r: Int): Int = if (l < r) l else r

    def interp(muwae: MUWAE, env: Map[String, List[Int]]): List[Int] = muwae match{
      case Num(n) => n
      case Add(l, r) => binOp(add, interp(l, env), interp(r, env))
      case Sub(l, r) => binOp(sub, interp(l, env), interp(r, env))
      case With(x, i, n) => interp(n, env + (x -> interp(i, env)))
      case Id(x) => env getOrElse(x, error(s"$x is a free identifier!"))
      case Min(l, m, r) => binOp(min, binOp(min, interp(l, env), interp(m, env)), interp(r, env))
      case Max(l, m, r) => binOp(max, binOp(max, interp(l, env), interp(m, env)), interp(r, env))
    }

    interp(MUWAE(str), Map())
  }

  def tests: Unit = {
    test(run("{+ 3 7}"), List(10))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{min 3 4 5}"), List(3))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
    // 1. Num(n)
    test(run("1"), List(1))
    test(run("{1 2}"), List(1, 2))

    // 2. Add(l, r)
    test(run("{+ 3 {2 4}}"), List(5, 7))
    test(run("{+ 10 {3 5}}"), List(13, 15))
    test(run("{+ {1 2} {3 4}}"), List(4, 5, 5, 6))

    // 3. Sub(l, r) --> not needed! (because it is logically equivalent to Add)

    // 4. Min, Max --> not needed! (because Min, Max are logically equivalent,
    // and all the logically possible cases have been tested above i.e. line 44~47)

    // 5. Cases with Nil
    test(run("{}"), Nil)
    test(run("{+ 2 {}}"), Nil)
    test(run("{- 2 {}}"), Nil)
    test(run("{min 1 {} {2 3}}"), Nil)
    test(run("{max 1 {} {2 3}}"), Nil)

    // 6. Simple expressions containing With, Id --> not needed! (because the only possible logically-simple case
    // has already been tested above i.e. line 43)

    // 7. Complicated expressions containing With, Id
    // 7.1 Some examples from the lecture note
    test(run("{+ {with {x {+ 1 2}} {+ x x}} {with {x {- 4 3}} {+ x x}}}"), List(8))
    test(run("{+ {with {x {+ 1 2}} {+ x x}} {with {y {- 4 3}} {+ y y}}}"), List(8))
    test(run("{with {x {+ 1 2}} {with {x {- 4 3}} {+ x x}}}"), List(2))
    test(run("{with {x {+ 1 2}} {with {y {- 4 3}} {+ x x}}}"), List(6))
    test(run("{with {x 1} {with {x 2} x}}"), List(2))
    test(run("{with {x 1} {+ {with {x 2} x} x}}"), List(3))

    // 7.2 Free identifier error
    testExc(run("x"), "x is a free identifier!")
    testExc(run("{+ x 2}"), "x is a free identifier!")
    testExc(run("{with {x 1} y}"), "y is a free identifier!")
    testExc(run("{with {x 1} {+ y x}}"), "y is a free identifier!")

    // 7.3 Miscellaneous
    test(run("{with {x {1 2}} {+ x x}}"), List(2, 3, 3, 4))
    test(run("{max 3 {with {x {1 2}} {+ x x}} {min {1 2} 3 1}}"), List(3, 3, 3, 3, 3, 3, 4, 4))
  }
}
