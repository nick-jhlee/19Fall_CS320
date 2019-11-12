package cs320

package object hw07 extends Homework07 {

  trait KXCFAEValue
  case class NumV(n: Int) extends KXCFAEValue
  case class CloV(param: List[String], body: KXCFAE, env: Env) extends KXCFAEValue
  case class ContV(k: Cont) extends KXCFAEValue

  type Env = Map[String, KXCFAEValue]
  type Cont = KXCFAEValue => KXCFAEValue

  def numVAdd(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }

  def numVSub(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }

  def run(str: String): String = {
    def interp(kxcfae: KXCFAE, env: Env, k: Cont): KXCFAEValue = kxcfae match {
      case Num(n) => k(NumV(n))

      case Add(left, right) =>
        interp(left, env, lv =>
          interp(right, env, rv =>
            k(numVAdd(lv, rv))))

      case Sub(left, right) =>
        interp(left, env, lv =>
          interp(right, env, rv =>
            k(numVSub(lv, rv))))

      case Id(name) => k(env.getOrElse(name, error(s"free identifier: $name")))

      case Fun(params, body) => k(CloV(params, body, env))

      case App(fun, args) => {
        def ListEnv(params: List[String], args: List[KXCFAEValue], env: Env): Env = {
          if (params.length != args.length)
            error(s"wrong arity")
          else if (params.length == 1)
            env + (params.head -> args.head)
          else if (params.length == 0)
            env
          else
            ListEnv(params.drop(1), args.drop(1), env + (params.head -> args.head))
        }


        interp(fun, env, fv => {
          def f(fv: KXCFAEValue, a: List[KXCFAE], env: Env, tmp: List[KXCFAEValue]): KXCFAEValue = {
            if (a.length > 1) {
              interp(a.head, env, av => f(fv, a.drop(1), env, tmp :: av)._1)
            }
            else {
              interp(a.head, env, av =>
              fv match {
                case CloV(p, b, fenv) =>
                  interp(b, ListEnv(p.dropRight(1), tmp, fenv) + (p.last -> av), k)
                case v =>
                  error(s"not a closure: $v")
              })
            }
          }
          f(fv, args, env, List())
        })
      }


      case If0(cond, thenE, elseE) =>
        val bool = interp(cond, env, k)
        if (bool == NumV(0))
          interp(thenE, env, k)
        else
          interp(elseE, env, k)

      case Withcc(name, body) => interp(body, env + (name -> ContV(k)), k)

      case Try(tryE, catchE) => ???

      case Throw => ???

      //      case class Try(tryE: KXCFAE, catchE: KXCFAE) extends KXCFAE               //     | {try e catch e}
//      case object Throw extends KXCFAE                                          //     | {throw}
    }

    interp(KXCFAE(str), Map(), x => x) match {
      case NumV(n) => n.toString
      case CloV(p, b, e) => "function"
      case ContV(k) => "continuation"
    }
  }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")

    /* Write your own tests */
  }
}
