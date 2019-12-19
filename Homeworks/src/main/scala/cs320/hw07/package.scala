package cs320

package object hw07 extends Homework07 {

  trait KXCFAEValue
  case class NumV(n: Int) extends KXCFAEValue
  case class CloV(param: List[String], body: KXCFAE, env: Env) extends KXCFAEValue
  case class ContV(k: Cont) extends KXCFAEValue
  case object ThrowV extends KXCFAEValue

  type Env = Map[String, KXCFAEValue]
  type Cont = KXCFAEValue => KXCFAEValue

  def numVAdd(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case (ThrowV, _) => ThrowV
    case (_, ThrowV) => ThrowV
    case _ => error(s"not both numbers: $x, $y")
  }

  def numVSub(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case (ThrowV, _) => ThrowV
    case (_, ThrowV) => ThrowV
    case _ => error(s"not both numbers: $x, $y")
  }

  def run(str: String): String = {
    def interp(TRY: Boolean, kxcfae: KXCFAE, env: Env, k: Cont): KXCFAEValue = kxcfae match {
      case Num(n) => k(NumV(n))

      case Add(left, right) =>
        interp(TRY, left, env, lv =>
          interp(TRY, right, env, rv =>
            k(numVAdd(lv, rv))))

      case Sub(left, right) =>
        interp(TRY, left, env, lv =>
          interp(TRY, right, env, rv =>
            k(numVSub(lv, rv))))

      case Id(name) =>
//        print(s"Id: $name       $env\n")
        k(env.getOrElse(name, error(s"free identifier: $name")))

      case Fun(params, body) =>
//        print(s"Fun: $params   $body\n")
        k(CloV(params, body, env))

      case App(fun, args) => interp(TRY, fun, env, {
        case CloV(p, b, fenv) =>
          if (p.length != args.length)
            error("wrong arity")
          if (!TRY && b == Throw)
            error("no enclosing try-catch")
          def recur_interp1(args: List[KXCFAE], params: List[String], final_env: Env): KXCFAEValue = {
            if (params == Nil) {
              interp(TRY, b, final_env, k)
            } else {
              val a = args.head
              a match {
                case Throw => ThrowV
                case _ => interp(TRY, a, env, av =>
                  recur_interp1(args.tail, params.tail, final_env + (params.head -> av)))
              }
            }
          }
          recur_interp1(args, p, fenv)

        case ContV(kv) =>
          def recur_interp2(args: List[KXCFAE]): KXCFAEValue = {
            if (args == Nil) {
              error("Hmmm2...")
              ContV(kv)
            }
            //              print("ARGS is NIL...?\n")
            val a = args.head
            a match {
              case Throw => ThrowV
              case _ => interp(TRY, a, env, av => kv(av))
            }
          }
          recur_interp2(args)

        case ThrowV => ThrowV

        case v =>
          error(s"not a closure: $v")
      })

      case If0(cond, thenE, elseE) =>
//        print("If0\n")
        val bool = interp(TRY, cond, env, k)
        if (bool == ThrowV)
          ThrowV
        else
          interp(TRY,
            if (bool == NumV(0)) thenE else elseE,
            env, k
          )

      case Withcc(name, body) => interp(TRY, body, env + (name -> ContV(k)), k)

      case Try(tryE, catchE) =>
//        print(s"$tryE     $catchE\n")
        val t = interp(TRY = true, tryE, env, k)
//        print(s"t: $t\n")
        t match {
          case ThrowV => interp(TRY = false, catchE, env, k)
          case _ => t
        }

      case Throw => ThrowV
    }

    interp(TRY = false, KXCFAE(str), Map(), x => x) match {
      case NumV(n) => n.toString
      case CloV(_, _, _) => "function"
      case ContV(_) => "continuation"
      case ThrowV => error("no enclosing try-catch")
    }
  }

  def tests: Unit = {
    test(run("{withcc k k}"), "continuation")
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{{fun {x y z} {- y { + x z}}} 10 12 2}"), "0")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    testExc(run("{throw}"), "no enclosing try-catch")
//
//    /* Write your own tests */
    test(run("{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}"), "20")
    test(run("{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}"), "15")
//
//
    test(run("{withcc esc {try {+ {throw} {esc 3}} catch 4}}"), "4")
    test(run("{try {{fun {x y z} {a b c}} 1 2 {throw}} catch 0}"), "0")
    test(run("{try {if0 {throw} 3 4} catch 5}"), "5")
    test(run("{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}"), "-1")
//
    test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {zzz 10}}} catch 42}"), "42")

    test(run("{try 7 catch 8}"), "7")
    test(run("{try {throw} catch 8}"), "8")
    test(run("{try {+ 1 {throw}} catch 8}"), "8")
    test(run("{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}"), "8")
    test(run("{try {try {throw} catch 8} catch 9}"), "8")
    test(run("{try {try {throw} catch {throw}} catch 9}"), "9")
    test(run("{try {try 7 catch {throw}} catch 9}"), "7")
    test(run("{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")

    test(run("{fun {x} {fun {} x}}"), "function")
    test(run("{{{fun {x} {fun {} x}} 13}}"), "13")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")


    testExc(run("{{fun{x}{x}} 1 {throw}}"), "wrong arity")
    test(run("{+ {withcc k {k 5}} 4}"), "9")

    test(run("{withcc k {- 0 {k 100}}}"), "100")
    test(run("{withcc k {k {- 0 100}}}"), "-100")
    test(run("{withcc k {k {+ 100 11}}}"), "111")
    test(run("{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 300}"), "0")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")

    test(run("{{fun {f} {try {f 3} catch 8}} {fun {x} {throw}}}"), "8")
    test(run("{try {- 0 {withcc k {+ 3 {k {throw}}}}} catch 89}"), "89")
    test(run("{try {+ 3 {withcc k {+ 1000 {k {throw}}}}} catch 11}"), "11")

    test(run("{+ {try {- 10 {throw}} catch 3} 10}"), "13")

    test(run("{try {withcc a {+ 1 {withcc b {throw}}}} catch 10}"), "10")
    test(run("{try {- 0 {throw}} catch 5}"), "5")

    test(run("{{fun {x y z} {try {+ 1 {+ x {throw}}} catch {+ y z}}} 1 2 3}"), "5")
//
    test(run("{try {if0 0 {throw} {+ 1 2}} catch {if0 10 1 {try {throw} catch 54}}}"), "54")
    test(run("{{withcc esc {try {{withcc k {esc k}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
    test(run("{withcc foo {{fun {x y} {y x}} {+ 2 3} {withcc bar {+ 1 {bar foo}}}}}"), "5")
    test(run("{try {withcc zzz {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {zzz 10} {throw}}} catch 42}"), "10")
    test(run("{try {withcc zzz {{fun {x y z w} {+ {w {+ x y}} {+ {throw} z}}} 1 2 3 zzz}} catch 42}"), "3")
    test(run("{try {withcc x {+ {x 1} {throw}}} catch 0}"), "1")
    test(run("{+ 12 {withcc k {+ 1 {k {{fun {} 7}}}}}}"), "19")
//

    test(run("{try {try {throw} catch {try {throw} catch {try {throw} catch {+ {withcc k {try {throw} catch {k 0}}} {throw}}}}} catch 0}"), "0")
//
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    testExc(run("{throw}"), "no enclosing try-catch")
//
    test(run("{{fun {x y} x} {withcc esc {esc 1}} 2}"), "1")
    test(run(" {try {fun {x} {throw}} catch 1}"), "function")
    test(run("{try {fun {} {throw}} catch 1}"), "function")
    testExc(run("{{try {fun {} {throw}} catch 1}}"), "no enclosing try-catch")
    test(run("{ {fun {x} {x 3}} {fun {x} {+ 2 x}}}"),"5")
    test(run("{try {try {throw} catch {throw}} catch 1}"), "1")
    test(run("{withcc esc {{fun {x y} {+ {+ x 3} y}} {withcc k {try {k {esc {throw}}} catch {k 5}}} 7}}"), "15")
    test(run("{try {try {throw} catch {throw}} catch 1}"), "1")
    testExc(run("{try {throw} catch {throw}}"), "no enclosing try-catch")
    testExc(run("{{fun {x y z} x} {throw} 1 2 3}"), "wrong arity")
    testExc(run("{{fun {x y} y} {throw} 3}"), "no enclosing try-catch")
    testExc(run("{{fun {withcc x {x f}} {+ f 2}} 2}"), "identifier")
    testExc(run("{1 {throw}}"), "closure")
    test(run("{withcc x {try {+ {throw} {x 2}} catch {x 3}}}"), "3")
    testExc(run("{if0 {throw} 1 2}"), "no enclosing try-catch")
    test(run("{if0 1 {throw} 2}"), "2")
    testExc(run("{{fun{x} {x}} 1 {throw}}"), "wrong arity")
    test(run("{try {withcc A {{fun {x} {A x 1}} {throw}}} catch 3}"), "3")
    testExc(run("{+ 1 {throw}}"), "no enclosing try-catch")
    test(run("{withcc esc {{fun {x y} x} {esc 3} 1 }}"), "3")
    test(run("{withcc esc {{fun {x y} x} {esc 4} {esc 3}}}"), "4")
    test(run("{{try {fun {} 3} catch 1}}"), "3")

    testExc(run("{if0 {throw} 1 2}"), "no enclosing try-catch")
    test(run("{try {if0 {throw} 1 2} catch 3}"), "3")

//    test(run("{{withcc esc {try {{withcc k {try {esc k} catch {fun {x} {fun {y} 9}}}} 0} catch {fun {x} 8}}} {fun {x} {throw}}}"), "8")
//    test(run("{+ 999 {withcc done {{fun {f x} {f f x done}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "1099")
//    test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 100} {+ y {g g {- y 1} z}}}} 10}}}"), "11053")
//    test(run("{+ 999 {withcc done {{fun {f x} {f f x {fun {x} {if0 x {done {- 0 999}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {+ y {g g {- y 1} z}}}} 10}}}"), "0")
//    test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 110}"), "110")
//    test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {done y}}}} 10}}"), "2")
//    test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch {throw}}}} 10} catch 20110464}"), "20110464")
//    test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 10}"), "55")
//    test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 100} {+ y {g g {- y 1}}}}} 10}}"), "100")
//    test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {fun {x} 42}}}}} catch 4242}"), "4242")
//    test(run("{withcc esc {{try {withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} {throw}}}}} {fun {g y z} {if0 {- y 1} {z 1} {{g g {- y 1} z} 32}}} 4}} catch esc} 33}}"), "33")
//    test(run("{try {{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {fun {y} {throw}}}}} catch 4242}"), "4242")
//    test(run("{withcc done {{fun {f x} {f f x {fun {x} {if0 x {fun {y} {fun {x} {+ x y}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}}"), "64")
//    test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 3}} 5}"), "continuation")
//    test(run("{{withcc done {{fun {f x} {f f x {fun {x} {if0 x {withcc k {fun {x} {fun {x} {fun {x} k}}}} 10000}}}} {fun {g y z} {if0 {- y 1} {z 0} {{g g {- y 1} z} 32}}} 4}} {fun {y} {fun {y} {  fun {y} {fun {x} 42}}}}}"), "42")
//    test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}"), "54")
  }
}
