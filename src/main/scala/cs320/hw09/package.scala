package cs320

package object hw09 extends Homework09 {
  def notype(msg: Any): Nothing = error(s"no type: $msg")

  def mustSame(left: Type, right: Type): Type =
    if (same(left, right)) left
    else notype(s"$left is not equal to $right")

  def same(left: Type, right: Type): Boolean = (left, right) match {
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (ArrowT(ps1, r1), ArrowT(ps2, r2)) =>
      same(r1, r2) && (ps1 == ps2)
    case (IdT(x), IdT(y)) =>
      if (x == y) true
      else false
    case _ => false
  }

  def validType(ty: Type, tyEnv: TyEnv): Type = ty match {
    case NumT => ty
    case BoolT => ty
    case ArrowT(ps, r) =>
      ArrowT(ps.map(validType(_, tyEnv)), validType(r, tyEnv))
    case IdT(x) => ???
//      if (tyEnv.tbinds.contains(x)) ty
//      else notype(s"$x is a free type")
  }

  @scala.annotation.tailrec
  def addVar_multiple(tyEnv: TyEnv, params: List[(String, Type)], mutable: Boolean): TyEnv =
    if (params.isEmpty) tyEnv
    else addVar_multiple(tyEnv.addVar(params.head._1, params.head._2, mutable), params.tail, mutable)



  def typeCheck(e: Expr, tyEnv: TyEnv): Type = e match {
    case Num(_) => NumT

    case Bool(_) => BoolT

    case Add(left, right) =>
      mustSame(typeCheck(left, tyEnv), NumT)
      mustSame(typeCheck(right, tyEnv), NumT)
      NumT

    case Sub(left, right) =>
      mustSame(typeCheck(left, tyEnv), NumT)
      mustSame(typeCheck(right, tyEnv), NumT)
      NumT

    case Eq(left, right) =>
      mustSame(typeCheck(left, tyEnv), NumT)
      mustSame(typeCheck(right, tyEnv), NumT)
      BoolT

    case Lt(left, right) =>
      mustSame(typeCheck(left, tyEnv), NumT)
      mustSame(typeCheck(right, tyEnv), NumT)
      BoolT

    case Id(name) =>
      tyEnv.varMap.getOrElse(name, notype(s"$name is a free identifier"))

    case Fun(params, body) =>
      for (item <- params) validType(item._2, tyEnv)
      ArrowT(params.map(_._2), typeCheck(body, addVar_multiple(tyEnv, params, mutable = false)))

    case App(func, args) => typeCheck(func, tyEnv) match {
      case ArrowT(ps, r) =>
        if (ps == args) r
        else notype(s"Input types don't match")
      case _ => notype(s"$func is not an arrow type")
    }

    case Block(stmts, expr) =>
      if (stmts.isEmpty) typeCheck(expr, tyEnv)
      else typeCheck(Block(stmts.tail, expr), typeCheck(tyEnv, stmts.head))

    case Assign(name, expr) =>
      if (!tyEnv.mutables.contains(name))
        notype(s"$name is not a mutable type")
      typeCheck(expr, tyEnv.addVar(name, tyEnv.varMap.getOrElse(name, notype(s"$name is a free identifier")), mutable = true))

    case Match (expr, cases) => ???

    case IfThenElse(cond, thenE, elseE) =>
      mustSame(typeCheck(cond, tyEnv), BoolT)
      mustSame(typeCheck(thenE, tyEnv), typeCheck(elseE, tyEnv))
  }


  def typeCheck(tyEnv: TyEnv, stmt: Stmt): TyEnv = stmt match {
    case Val(_, name, ty, expr) =>
      validType(ty, tyEnv)
      tyEnv.addVar(name, mustSame(typeCheck(expr, tyEnv), ty), mutable = false)

    case Var(name, ty, expr) =>
      validType(ty, tyEnv)
      tyEnv.addVar(name, mustSame(typeCheck(expr, tyEnv), ty), mutable = true)

    case Def(name, params, retTy, body) =>
      for (item <- params) validType(item._2, tyEnv)
      validType(retTy, tyEnv)
      mustSame(typeCheck(body, addVar_multiple(tyEnv, List((name, ArrowT(params.map(_._2), retTy))) ++ params, mutable = false)), retTy)
      tyEnv.addVar(name, ArrowT(params.map(_._2), retTy), mutable = false)

    case Trait(name, cases) => ???
  }




  def numVAdd(x: Value, y: Value): NumV = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n + m)
    case _ => error("Type checking error?!")
  }

  def numVSub(x: Value, y: Value): NumV = (x, y) match {
    case (NumV(n), NumV(m)) => NumV(n - m)
    case _ => error("Type checking error?!")
  }

  def numVEq(x: Value, y: Value): BoolV = (x, y) match {
    case (NumV(n), NumV(m)) => BoolV(n == m)
    case _ => error("Type checking error?!")
  }

  def numVComp(x: Value, y: Value): BoolV = (x, y) match {
    case (NumV(n), NumV(m)) => BoolV(n < m)
    case _ => error("Type checking error?!")
  }

  def malloc(sto: Sto): Addr =
    sto.foldLeft(0) {
      case (max, (addr, _)) => math.max(max, addr)
    } + 1

  @scala.annotation.tailrec
  def addEnv_multiple(retenv: Env, env: Env, sto: Sto, args: List[Expr], params: List[String]): (Env, Sto) =
    if (args.isEmpty) (retenv, sto)
    else  {
      val (v, s) = interp(args.head, env, sto)
      addEnv_multiple(retenv + (params.head -> v), env, s, args.tail, params.tail)
    }

  @scala.annotation.tailrec
  def addVarV_multiple(retvalues: List[Value], env: Env, sto: Sto, args: List[Expr]): (List[Value], Sto) =
    if (args.isEmpty) (retvalues, sto)
    else  {
      val (v, s) = interp(args.head, env, sto)
      addVarV_multiple(retvalues ++ List(v), env, s, args.tail)
    }


  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = e match {
    case Num(num) => (NumV(num), sto)

    case Bool(bool) => (BoolV(bool), sto)

    case Add(left, right) =>
      val (lv, ls) = interp(left, env ,sto)
      val (rv, rs) = interp(right, env, ls)
      (numVAdd(lv, rv), rs)

    case Sub(left, right) =>
      val (lv, ls) = interp(left, env ,sto)
      val (rv, rs) = interp(right, env, ls)
      (numVSub(lv, rv), rs)

    case Eq(left, right) =>
      val (lv, ls) = interp(left, env ,sto)
      val (rv, rs) = interp(right, env, ls)
      (numVEq(lv, rv), rs)

    case Eq(left, right) =>
      val (lv, ls) = interp(left, env ,sto)
      val (rv, rs) = interp(right, env, ls)
      (numVComp(lv, rv), rs)

    case Id(name) =>
      val a = env.getOrElse(name, error(s"$name is a free identifier"))
      a match {
      case AddrV(addr) =>
        val v1 = sto.getOrElse(addr, error(s"$a not in sto"))
        v1 match {
          case ExprV(expr, e) =>
            val (v2, s2) = interp(expr, e, sto)
            (v2, s2 + (addr -> v2))
          case _ => (v1, sto)
        }

      case _ => (a, sto)
    }

    case Fun(params, body) => (CloV(params.map(_._1), body, env), sto)

    case App(func, args) =>
      val (v0, s0) = interp(func, env, sto)
      v0 match {
        case CloV(params, body, env) =>
          val (e, s) = addEnv_multiple(Map(), env, s0, args, params)
          interp(body, e, s)

        case ConstructorV(name) =>
          val (list_v, s) = addVarV_multiple(List(), env, sto, args)
          (VariantV(name, list_v), s)

        case _ => error("Type checking error")
      }

    case Block(stmts, expr) =>
      if (stmts.isEmpty)  interp(expr, env, sto)
      else {
        val (e, s) = interp((env, sto), stmts.head)
        interp(Block(stmts.tail, expr), e, s)
      }

    case Assign(name, expr) =>
      val a = env.getOrElse(name, error(s"$name not in env"))
      val (v, s) = interp(expr, env, sto)
      a match {
        case AddrV(addr) => (v, s + (addr -> v))
        case _ => error(s"$a not addr")
      }

    case Match (expr, cases) => ???

    case IfThenElse(cond, thenE, elseE) => interp(cond, env, sto) match {
      case (BoolV(true), s_true) => interp(thenE, env, s_true)
      case (BoolV(false), s_false) => interp(elseE, env, s_false)
      case _ => error("Type checking error?!")
    }
  }


  def interp(pair: (Env, Sto), stmt: Stmt): (Env, Sto) = stmt match {
    case Val(isLazy, name, _, expr) =>
      if (isLazy) {
        val (v, s) = interp(expr, pair._1, pair._2)
        (pair._1 + (name -> v), s)
      } else {
        val a = malloc(pair._2)
        (pair._1 + (name -> AddrV(a)), pair._2 + (a -> ExprV(expr, pair._1)))
      }

    case Var(name, _, expr) =>
      val (v, s) = interp(expr, pair._1, pair._2)
      val a = malloc(s)
      (pair._1 + (name -> AddrV(a)), s + (a -> v))

    case Def(name, params, _, body) => ???
//      val env: Env = pair._1 + (name -> CloV(params.map(_._1), body, env: Env))
//      (env, pair._2)

    case Trait(name, cases) => ???

  }


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
