package cs320

package object hw03 extends Homework03 {

  //From Lecture 7
  trait MRFWAEValue
  case class NumV(n: Int) extends MRFWAEValue
  case class CloV(param: List[String], body: MRFWAE, env: Env) extends MRFWAEValue
  case class Record(rec: Map[String, MRFWAEValue]) extends MRFWAEValue
  type Env = Map[String, MRFWAEValue]
  type Rec = Map[String, MRFWAEValue]


  def numVAdd(x: MRFWAEValue, y: MRFWAEValue): MRFWAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }

  def numVSub(x: MRFWAEValue, y: MRFWAEValue): MRFWAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }

  // Interpreter for MRFWAE
  def run(str: String): String = {

    def interp(mrwfwae: MRFWAE, env: Env, rec: Rec): MRFWAEValue = mrwfwae match{
      case Num(n) => NumV(n)
      case Add(l, r) => numVAdd(interp(l, env, rec), interp(r, env, rec))
      case Sub(l, r) => numVSub(interp(l, env, rec), interp(r, env, rec))
      case With(x, i, n) => interp(n, env + (x -> interp(i, env, rec)), rec)
      case Id(x) => env getOrElse(x, error(s"$x is a free identifier!"))

      case App(f, a) => interp(f, env, rec) match {
        case CloV(x, b, fenv) => {
          def ListEnv(params: List[String], args: List[MRFWAE], env: Env): Env = {
            if (params.length != args.length)
              error(s"wrong arity")
            else if (params.length == 1)
              env + (params.head -> interp(args.head, env, rec))
            else if (params.length == 0)
              Map()
            else
              ListEnv(params.drop(1), args.drop(1), env + (params.head -> interp(args.head, env, rec)))
          }

          interp(b, fenv ++ ListEnv(x, a, fenv), rec)
        }

        case v => error(s"not a closure: $v")
      }
      case Fun(x, b) => CloV(x, b, env)

      case Rec(r) => {
        def k = r.keys
        def v = r.values
        def tmp1(mrwfwae_tmp: MRFWAE) = interp(mrwfwae_tmp, env, rec)

        Record((k zip v.map(tmp1)).toMap)
      }

      case Acc(e, n) => {
        interp(e, env, rec) match {
          case Record(r) => r getOrElse(n, error(s"no such field"))
          case v => error(s"no such field")
        }
      }
    }

    interp(MRFWAE(str), Map(), Map()) match {
      case NumV(n) => n.toString
      case CloV(p, b, e) => "function"
      case Record(rec) => "record"
    }
  }


  def tests: Unit = {
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */
    // From 지웅
  }
}
