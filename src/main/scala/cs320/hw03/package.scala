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
    test(run("{- 20 {{fun {x} {+ x x}} 17}}"), "-14")
    test(run("{with {f {fun {r} {+ 2 r}}} {f 3}}"), "5")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {f {fun {a b} {+ a b}}} {with {g {fun {x} {- x 5}}} {with {x {f 2 5}} {g x}}}}"), "2")
    test(run("{with {f {fun {x y} {+ x y}}} {f 1 2}}"), "3")
    test(run("{with {f {fun {} 5}} {+ {f} {f}}}"), "10")
    test(run("{with {h {fun {x y z w} {+ x w}}} {h 1 4 5 6}}"), "7")
    test(run("{with {f {fun {} 4}} {with {g {fun {x} {+ x x}}} {with {x 10} {- {+ x {f}} {g 4}}}}}"), "6")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}"), "3")
    test(run("{with {f {fun {a b} {+ {access a a} b}}} {with {g {fun {x} {+ 5 x}}} {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}"), "17")
    test(run("{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}} {access {f 1 2 3 4 5} c}}"), "3")
    test(run("{with {f {fun {a b c} {record {a a} {b b} {c c}}}} {access {f 1 2 3} b}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} y}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} d}}"), "2")
    test(run("{with {f {fun {x} {+ 5 x}}} {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}"), "8")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{record {a 10}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {+ 1 2}}} a}"), "3")
    test(run("{fun {x} x}"), "function")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{record {a 10}}"), "record")
    test(run("{record {a {- 2 1}}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {- 2 1}}} a}"), "1")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y y}}"), "2")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y z}}"), "3")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
  }
}
