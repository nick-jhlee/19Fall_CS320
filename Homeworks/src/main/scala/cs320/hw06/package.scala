package cs320

package object hw06 extends Homework06 {

  // From Lecture 12

  type Addr = Int

  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(param: String, body: SRBFAE, env: Env) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  case class Record(rec: Map[String, Addr]) extends SRBFAEValue
  type Env = Map[String, Addr]
  type Sto = Map[Addr, SRBFAEValue]
  type Rec = Map[String, Addr]

  // From Lecture 11
  def malloc(sto: Sto): Addr =
    sto.foldLeft(0) {
      case (max, (addr, _)) => math.max(max, addr)
    } + 1

  def lookup_env(str: String, env: Env): Addr = env getOrElse(str, error(s"free identifier: $str"))
  def lookup_rec(str: String, rec: Rec): Addr = rec getOrElse(str, error(s"no such field: $str"))
  def storeLookup(addr: Addr, sto: Sto): SRBFAEValue = sto getOrElse(addr, error(s"not a valid address: $addr"))


  def numVAdd(x: SRBFAEValue, y: SRBFAEValue): SRBFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error(s"not both numbers: $x, $y")
  }

  def numVSub(x: SRBFAEValue, y: SRBFAEValue): SRBFAEValue = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error(s"not both numbers: $x, $y")
  }

  // Interpreter for MRFWAE
  // From here, use store/record-passing implementation
  // Use call-by-value implementation.
  def run(str: String): String = {

    def interp(srbfae: SRBFAE, env: Env, rec: Rec, sto: Sto): (SRBFAEValue, Sto) = srbfae match {
      case Num(num) => (NumV(num), sto)

      case Add(left, right) =>
        val (lv, ls) = interp(left, env, rec, sto)
        val (rv, rs) = interp(right, env, rec, ls)
        (numVAdd(lv, rv), rs)

      case Sub(left, right) =>
        val (lv, ls) = interp(left, env, rec, sto)
        val (rv, rs) = interp(right, env, rec, ls)
        (numVSub(lv, rv), rs)

      case Id(name: String) => (storeLookup(lookup_env(name, env), sto), sto)

      case Fun(param: String, body: SRBFAE) =>
        (CloV(param, body, env), sto)

      case App(fun: SRBFAE, arg: SRBFAE) =>
        val (fv, fs) = interp(fun, env, rec, sto)
        val (av, as) = interp(arg, env, rec, fs)
        fv match {
          case CloV(x, b, fenv) =>
            val addr = malloc(as)
            interp(b, fenv + (x -> addr), rec, as + (addr -> av))
          case _ =>
            error(s"not a closure: $fv")
        }

      case NewBox(expr: SRBFAE) =>
        val (v, s) = interp(expr, env, rec, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v))

      case SetBox(box: SRBFAE, expr: SRBFAE) =>
        val (bv, bs) = interp(box, env, rec, sto)
        bv match {
          case BoxV(addr) =>
            val (v, s) = interp(expr, env, rec, bs)
            (v, s + (addr -> v))
          case _ =>
            error(s"not a box: $bv")
        }

      case OpenBox(box: SRBFAE) =>
        val (bv, bs) = interp(box, env, rec, sto)
        bv match {
          case BoxV(addr) =>
            (storeLookup(addr, bs), bs)
          case _ =>
            error(s"not a box: $bv")
        }

      case Seqn(left: SRBFAE, right: List[SRBFAE]) =>
        val (lv, ls) = interp(left, env, rec, sto)
        right match {
          case List() =>
            (lv, ls)
          case _ =>
            val head = right.head
            val tail = right.tail
            interp(Seqn(head, tail), env, rec, ls)
        }

      case Rec(fields) =>
        fields match {
          case List() =>
            (Record(rec), sto)
          case _ =>
            val (headl, headr) = fields.head
            val (headr_v, headr_s) = interp(headr, env, rec, sto)
            val tail = fields.tail
            val addr = malloc(headr_s)
            interp(Rec(tail), env, rec + (headl -> addr), headr_s + (addr -> headr_v))
        }

      case Get(record, field) =>
        val (v, s) = interp(record, env, rec, sto)
        v match {
          case Record(rec) =>
            rec getOrElse(field, error(s"no such field"))
            (storeLookup(lookup_rec(field, rec), s), s)
          case _ =>
            error(s"not a Record: $v")
        }

      case Set(record, field, expr) =>
        val (r, rs) = interp(record, env, rec, sto)
        r match {
          case Record(rec) =>
            val addr = lookup_rec(field, rec)
            val (v, vs) = interp(expr, env, rec, rs)
            (v, vs + (addr -> v))
          case _ =>
            error(s"not a box: $r")
        }
    }

    interp(SRBFAE(str), Map(), Map(), Map())._1 match {
      case NumV(n) => n.toString
      case CloV(p, b, e) => "function"
      case BoxV(a) => "box"
      case Record(rec) => "record"
    }
  }


  def tests: Unit = {
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{get {rec {x 1} {x 2}} x}"), "2")
    test(run("{{fun {r} {seqn {set r x 5}}} {rec {x 1}}}"), "5")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")

    /* Write your own tests */
    test(run("{seqn 1 2}"), "2")
    test(run("{{fun {b} {openbox b}} {newbox 10}}"), "10")
    test(run("{{fun {b} {seqn {setbox b 12} {openbox b}}} {newbox 10}}"), "12")
    test(run("{{fun {b} {seqn {setbox b 12} {openbox b}}} {newbox 10}}"), "12")
    test(run("{{fun {b} {openbox b}} {seqn {newbox 9} {newbox 10}}}"), "10")
    test(run("{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"), "9")
    test(run("{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}"), "2")
    test(run("{{fun {b} {seqn {setbox b {+ 2 {openbox b}}} {setbox b {+ 3 {openbox b}}} {setbox b {+ 4 {openbox b}}} {openbox b}}} {newbox 1}}"), "10")
    test(run("{{fun {r} {get r x}} {rec {x 1}}}"), "1")
    test(run("{{{{{fun {g} {fun {s} {fun {r1} {fun {r2} {+ {get r1 b} {seqn {{s r1} {g r2}} {+ {seqn {{s r2} {g r1}} {get r1 b}} {get r2 b}}}}}}}} {fun {r} {get r a}}} {fun {r} {fun {v} {set r b v}}}} {rec {a 0} {b 2}}} {rec {a 3} {b 4}}}"), "5")
  }
}
