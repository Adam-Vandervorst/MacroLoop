package be.adamv.macroloop.staging

//import be.adamv.macroloop.be.adamv.macroloop.macros.untuple

import scala.quoted.*
import scala.quoted.staging.{Compiler, run, withQuotes}

given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)


object ConstantTuple:
  def mapPF[Tup <: NonEmptyTuple : Type, R : Type](t: Expr[Tup], f: Expr[PartialFunction[Any, R]])(using p: Quotes): Expr[Tuple.Map[Tup, [_] =>> R]] =
//    val bseq = untuple[Any](t)

//    def inlinedDefDef(t: Term) =
//      t match
//        case Inlined(_, _, Block(t::Nil, _)) => t
//
//    def DefDefBody(t: Statement) =
//      t match
//        case DefDef(_, _::Nil, _, Some(r)) => r
//
//    def evalMatch(t: Statement, a: Term) =
//      t match
//        case Match(_, cs) =>
//          Match(a, cs)

//    val e: Quotes ?=> Expr[R] = evalMatch(DefDefBody(inlinedDefDef(f.asTerm)), bseq.head.asTerm).asExprOf[Option[Any]]
//    println(e.show)
//    val e =
//    println(run(('{ $f($t.head) }): Quotes ?=> Expr[R]))

//    val x: Quotes ?=> Expr[Any] = (q: Quotes) ?=> {
//      import q.reflect.{asTerm as asTerm_q}
//      val qt = t.asTerm_q.asExpr
////      val qf = f.asTerm_q.asExprOf[PartialFunction[Any, Any]]
//      '{ val a = $qt; a }
//    }
//    println(run(x))

//    import p.reflect.{asTerm as asTerm_p}
//    val c = Expr(2)
//    val x: Quotes ?=> Expr[Int] = (q: Quotes) ?=> {
//      import q.reflect.{asTerm as asTerm_q}
//      val eq = c.asTerm_q.asExprOf[Int]
//      '{ val a = $eq; a*a }
//    }
//    println(run(x))

//    println(run('{ (bseq).map({
//      case null => None
//      case x => Some(x)
//    }) }))
//    val x: Quotes ?=> Expr[Int] = '{ val a = 2; a*a }
//    println(run(x))

//    println(run('{ val a = 2; a*a }))

//    Expr(rseq)
    ???
