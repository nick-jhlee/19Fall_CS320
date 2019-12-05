package cs320

package object hw09 extends Homework09 {
  def typeCheck(e: Expr, tyEnv: TyEnv): Type = ???
  def typeCheck(tyEnv: TyEnv, stmt: Stmt): TyEnv = ???
  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = ???
  def interp(pair: (Env, Sto), stmt: Stmt): (Env, Sto) = ???
  def tests: Unit = {
    test(run("""
      var x: Int = 1
      val y: Int = (x = 3)
      x + y
    """), "6")
    test(run("""
      var x: Int = 1
      lazy val y: Int = (x = 3)
      x + y + x
    """), "7")
    test(run("""
      var x: Int = 0
      lazy val y: Int = (x = x + 1)
      val z: Int = y + y + y + y
      z"""), "4")
    testExc(run("""val x: Int = 42; x = 24"""), "")
    test(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)

      def interp(e: AE): Int = e match {
        case Num(n) => n
        case Add(l, r) => interp(l) + interp(r)
        case Sub(l, r) => interp(l) - interp(r)
      }

      interp(Add(Num(2), Sub(Num(3), Num(1))))
    """), "4")
    test(run("""
      trait Tree
      case class Leaf(Int)
      case class Node(Tree, Tree)

      def max(l: Int, r: Int): Int =
        if (l < r) r else l

      def depth(e: Tree): Int = e match {
        case Leaf(n) => 1
        case Node(l, r) => max(depth(l), depth(r)) + 1
      }

      depth(Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4)))
    """), "4")
  }
}
