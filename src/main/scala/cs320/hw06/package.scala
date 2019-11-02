package cs320

package object hw06 extends Homework06 {

  // From Lecture 12

  type Addr = Int

  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(param: String, body: SRBFAE, env: Env) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  case class Record(rec: Map[String, SRBFAEValue]) extends SRBFAEValue
  type Env = Map[String, Addr]
  type Sto = Map[Addr, SRBFAEValue]
  type Rec = Map[String, SRBFAEValue]

  // From Lecture 11
  def malloc(sto: Sto): Addr =
    sto.foldLeft(0) {
      case (max, (addr, _)) => math.max(max, addr)
    } + 1

  def lookup(str: String, env: Env): Addr = env getOrElse(str, error(s"free identifier: $str"))
  def storeLookup(addr: Addr, sto: Sto) = sto getOrElse(addr, error(s"not a valid address: $addr"))


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

    def interp(srbfae: SRBFAE, env: Env, rec: Rec, sto: Sto): (SRBFAEValue, Sto, Rec) = srbfae match {
      case Num(num) => (NumV(num), sto, rec)

      case Add(left, right) =>
        val (lv, ls, lr) = interp(left, env, rec, sto)
        val (rv, rs, rr) = interp(right, env, lr, ls)
        (numVAdd(lv, rv), rs, rr)

      case Sub(left, right) =>
        val (lv, ls, lr) = interp(left, env, rec, sto)
        val (rv, rs, rr) = interp(right, env, lr, ls)
        (numVSub(lv, rv), rs, rr)

      case Id(name: String) => (storeLookup(lookup(name, env), sto), sto, rec)

      case Fun(param: String, body: SRBFAE) =>
//        print("CLOV\n")
        (CloV(param, body, env), sto, rec)

      case App(fun: SRBFAE, arg: SRBFAE) =>
        val (fv, fs, fr) = interp(fun, env, rec, sto)
        val (av, as, ar) = interp(arg, env, fr, fs)
//        print(s"APP: $av\n")
        fv match {
          case CloV(x, b, fenv) =>
            val addr = malloc(as)
            interp(b, fenv + (x -> addr), ar, as + (addr -> av))
          case _ =>
            error(s"not a closure: $fv")
        }

      case NewBox(expr: SRBFAE) =>
        val (v, s, r) = interp(expr, env, rec, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v), r)

      case SetBox(box: SRBFAE, expr: SRBFAE) =>
        val (bv, bs, br) = interp(box, env, rec, sto)
        bv match {
          case BoxV(addr) =>
            val (v, s, r) = interp(expr, env, br, bs)
            (v, s + (addr -> v), r)
          case _ =>
            error(s"not a box: $bv")
        }

      case OpenBox(box: SRBFAE) =>
        val (bv, bs, br) = interp(box, env, rec, sto)
        bv match {
          case BoxV(addr) =>
            (storeLookup(addr, bs), bs, br)
          case _ =>
            error(s"not a box: $bv")
        }

      case Seqn(left: SRBFAE, right: List[SRBFAE]) =>
        val (lv, ls, lr) = interp(left, env, rec, sto)
        right match {
          case List() =>
            (lv, ls, lr)
          case _ =>
            val head = right.head
            val tail = right.tail
            interp(Seqn(head, tail), env, lr, ls)
        }

      case Rec(fields) =>
//        print("REC\n")
        fields match {
          case List() =>
            (Record(rec), sto, rec)
          case _ =>
            val (headl, headr) = fields.head
            val headr_v = interp(headr, env, rec, sto)._1
            val tail = fields.tail
            interp(Rec(tail), env, rec + (headl -> headr_v), sto)
        }

      case Get(record, field) =>
        val (v, s, r) = interp(record, env, rec, sto)
        (r getOrElse(field, error(s"no such field")), s, r)
//        v match {
//          case Record(rec) =>
//            val tmp = rec getOrElse(field, error(s"no such field"))
//            print(s"GET: $tmp    $rec      $r\n")
//            (tmp, s, r)
//          case _ =>
//            error(s"not a Record: $v")
//        }

      case Set(record, field, expr) =>
        val (r, rs, rr) = interp(record, env, rec, sto)
        r match {
          case Record(rec) =>
            rec getOrElse(field, error(s"no such field"))
            val (v, vs, vr) = interp(expr, env, rec, rs)
            val tmp = rec + (field -> v)
//            print(s"SET: $tmp\n")
            (v, vs, rec + (field -> v))
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
    test(run("{{fun {r} {seqn {set r x 5}}} {rec {x 1}}}"), "5")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")

    /* Write your own tests */
  }
}
