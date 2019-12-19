package cs320

package object hw01 extends Homework01 {
  // 1. Primitives (20 pts)
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a * b * c
  def concat(x: String, y: String): String = x.concat(y)

  // 2. Function Values (30 pts)
  def addN(n: Int): Int => Int = {
    def addN_return(x: Int): Int = x + n
    addN_return
  }
  def twice(f: Int => Int): Int => Int = {
    def twice_return(x: Int): Int = f(f(x))
    twice_return
  }
  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    def compose_return(x: Int): Int = f(g(x))
    compose_return
  }

  // 3. Data Structures (50 pts)
  // 3.1. Lists (20 pts)
  def double(l: List[Int]): List[Int] = l.map(_ * 2)
  def sum(l: List[Int]): Int = l.sum

  // 3.2. Maps (10 pts)
  def getKey(m: Map[String, Int], s: String): Int = m getOrElse(s, error(s"Error! $s not found!"))

  // 3.3. User-defined Structures (20 pts)
  def countLeaves(t: Tree): Int = t match {
    case Branch(t1, n, t2) => countLeaves(t1) + countLeaves(t2)
    case Leaf(n) => 1
  }
  def flatten(t: Tree): List[Int] = t match {
    case Branch(t1, n, t2) => flatten(t1) ++ List(n) ++ flatten(t2)
    case Leaf(n) => List(n)
  }

  def tests: Unit = {
    test(concat("abc", "def"), "abcdef")
    test(addN(5)(3), 8)
    test(addN(5)(42), 47)
    test(twice(addN(3))(2), 8)
    test(twice(addN(3))(7), 13)
    test(compose(addN(3), addN(4))(5), 12)
    test(compose(addN(3), addN(4))(11), 18)

    val l: List[Int] = List(1, 2, 3)
    test(double(l), List(2, 4, 6))
    test(double(double(l)), List(4, 8, 12))

    test(sum(List(1,2,3)), 6)
    test(sum(List(4,2,3,7,5)), 21)

    val m: Map[String, Int] = Map("Ryu" -> 42, "PL" -> 37)
    test(getKey(m, "Ryu"), 42)
    test(getKey(m, "PL"), 37)
    testExc(getKey(m, "CS320"), "CS320")

    val tree: Tree = Branch(Leaf(1), 2, Branch(Leaf(3), 4, Leaf(5)))
    test(countLeaves(tree), 3)
    test(flatten(tree), List(1, 2, 3, 4, 5))

    /* Write your own tests */
    // 1. Primitives
    test(volumeOfCuboid(1, 2, 3), 6)
    test(volumeOfCuboid(0, 2, 3), 0)
    test(volumeOfCuboid(1, 0, 3), 0)
    test(volumeOfCuboid(1, 2, 0), 0)

    // cases with empty string
    test(concat("", ""), "")
    test(concat("", "abc"), "abc")
    test(concat("abc", ""), "abc")

    // 2. Function Values
    val test_f1: Int => Int = addN(0)
    val test_f2: Int => Int = addN(1)
    val test_f3: Int => Int = addN(-1)
    test(test_f1(0), 0)
    test(test_f1(1), 1)
    test(test_f1(-1), -1)
    test(test_f2(0), 1)
    test(test_f2(1), 2)
    test(test_f2(-1), 0)
    test(test_f2(-2), -1)
    test(test_f3(0), -1)
    test(test_f3(1), 0)
    test(test_f3(2), 1)
    test(test_f3(-1), -2)

    val twice_f2: Int => Int = twice(test_f2)
    test(twice_f2(1), 3)

    val compose_f4: Int => Int = compose(test_f2, twice_f2)
    test(compose_f4(2), 5)

    // test for commutative property
    val compose_f4_2: Int => Int = compose(twice_f2, test_f2)
    test(compose_f4(2), compose_f4_2(2))

    // 3. Data Structures
    // 3.1. Lists
    // case with empty list
    val empty_list: List[Int] = List()
    test(double(empty_list), List())
    test(sum(empty_list), 0)

    // 3.2. Maps
    // case with empty map
    val empty_map: Map[String, Int] = Map()
    testExc(getKey(empty_map, "CS320"), "CS320")

    // 3.3. User-defined Structures
    val single_Leaf: Tree = Leaf(1)
    test(countLeaves(single_Leaf), 1)
    test(flatten(single_Leaf), List(1))

    // case with tree whose nodes are not in an increasing order
    val weird_tree: Tree = Branch(Leaf(5), 1, Leaf(2))
    test(countLeaves(weird_tree), 2)
    test(flatten(weird_tree), List(5, 1, 2))
  }
}
