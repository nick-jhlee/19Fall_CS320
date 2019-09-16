package cs320

package object ex01 extends Exercise01 {
  // Problem 1
  def freeIds(expr: WAE): Set[String] = ???

  // Problem 2
  def bindingIds(expr: WAE): Set[String] = ???

  // Problem 3
  def boundIds(expr: WAE): Set[String] = ???

  // Tests
  def tests: Unit = {
    test(freeIds(WAE("{with {x 1} {+ x y}}")), Set("y"))
    test(freeIds(WAE("{with {z 2} 1}")), Set())
    test(bindingIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(bindingIds(WAE("{with {z 2} 1}")), Set("z"))
    test(boundIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(boundIds(WAE("{with {z 2} 1}")), Set())

    /* Write your own tests */
  }
}
