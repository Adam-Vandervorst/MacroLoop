package macroloop.macros

import quoted.*
import scala.annotation.tailrec
import compiletime.*

def showImpl(e: Expr[Any])(using Quotes): Expr[String] =
  Expr(e.show)

def untuple[B : Type](e: Expr[Tuple])(using Quotes): Seq[Expr[B]] =
  import quotes.reflect.*
  def rec(tree: Term): Seq[Expr[B]] = tree match
    case Repeated(elems, _) => elems.map(_.asExprOf[B])
    case Typed(e, _) => rec(e)
    case Inlined(_, Nil, e) => rec(e)
    // UNSAFE, assume tuple
    case Apply(_, args) => args.map(_.asExprOf[B])
    case _ => report.errorAndAbort(s"couldn't untuple tree ${tree.show}")
  rec(e.asTerm)

//inline def spawnTuple[T, B]: Seq[B] = erasedValue[T] match
//  case _: EmptyTuple => Nil
//  case _: (h *: tail) => summonInline[h] *: spawnTuple[tail, B]

def tupleFromExprSmall[Tup <: NonEmptyTuple](e: Expr[Tup])(using Quotes): Tuple.Map[Tup, Expr] = e match
  case '{ Tuple1($x) } => Tuple(x).asInstanceOf
  case '{ Tuple2($x, $y) } => (x, y).asInstanceOf
  case '{ Tuple3($x, $y, $z) } => (x, y, z).asInstanceOf
  case '{ Tuple4($x, $y, $z, $w) } => (x, y, z, w).asInstanceOf


object Break extends Exception


object IntRangeImpl:
  def forEachUnrolled(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Unit])(using Quotes): Expr[Unit] =
    val range = Range(start.valueOrAbort, stop.valueOrAbort, step.valueOrAbort)
    val exprs = range.map(Expr(_)).map(i => Expr.betaReduce('{ $f($i) }))
    Expr.block(exprs.init.toList, exprs.last)

  def forEach(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Unit])(using Quotes): Expr[Unit] =
    '{
      var i = $start
      while i < $stop do
        ${
          Expr.betaReduce('{ $f(i) })
        }
        i += $step
    }

object ArrayIndexImpl:
  def forEach[T : Type](a: Expr[Array[T]], f: Expr[T => Unit])(using Quotes): Expr[Unit] =
    '{
      val size = $a.length
      var i = 0
      while i < size do
        ${
           Expr.betaReduce('{ $f($a(i)) })
        }
        i += 1
    }

  def forEachUnrolledN[T : Type](a: Expr[Array[T]], f: Expr[T => Unit], ne: Expr[Int])(using Quotes): Expr[Unit] =
    val n = ne.valueOrAbort
    '{
      val size = $a.length
      val r = size % ${Expr(n)}
      var i = 0
      while (i < r) {
        $f($a(i))
        i += 1
      }
      while (i < size) {
        ${ foreachInRange(0, n)(j => '{ $f($a(i + ${Expr(j)})) }) }
        i += ${Expr(n)}
      }
    }

  def forallException[T : Type](a: Expr[Array[T]], f: Expr[T => Boolean])(using Quotes): Expr[Boolean] =
    '{
      try
        val size = $a.length
        var i = 0
        while i < size do
          if ${ Expr.betaReduce('{ $f($a(i)) })} then i += 1
          else throw Break
        true
      catch
        case Break => false
    }

  def forallCondition[T : Type](a: Expr[Array[T]], f: Expr[T => Boolean])(using Quotes): Expr[Boolean] =
    '{
      val size = $a.length
      var b = true
      var i = 0
      while b && (i < size) do
        if ${ Expr.betaReduce('{ $f($a(i)) })} then i += 1
        else b = false
      b
    }

  private def foreachInRange(start: Int, end: Int)(f: Int => Expr[Unit])(using Quotes): Expr[Unit] = {
    @tailrec def unroll(i: Int, acc: Expr[Unit]): Expr[Unit] =
      if (i < end) unroll(i + 1, '{ $acc; ${f(i)} }) else acc
    if (start < end) unroll(start + 1, f(start)) else '{}
  }


object ConstantTupleImpl:
  def forEachUnrolled[Tup <: Tuple : Type](t: Expr[Tup], f: Expr[Any => Unit])(using Quotes): Expr[Unit] =
    ???


  /*
  type ReWrap[X, F[_], G[_]] = X match
    case G[t] => G[F[t]]
    case _ => Nothing

  given simplify_rewrap[T <: Tuple, F[_], G[_]]: (Tuple.Map[Tuple.Map[T, F], [X] =>> ReWrap[X, F, G]] =:= Tuple.Map[T, [X] =>> F[G[X]]]) = summon
  given simplify_map_map[T <: Tuple, F[_], G[_]]: (Tuple.Map[T, [X] =>> F[G[X]]] =:= Tuple.Map[Tuple.Map[T, G], F]) = summon
  given [T <: Tuple, F[_]]: (Tuple.Map[Tuple.InverseMap[T, F], F] =:= T) = summon

def mapUnrolled[Tup <: NonEmptyTuple : Type, F[_] : Type](t: Expr[Tup], f: Expr[[X] => X => F[X]])(using Quotes): Expr[Tuple.Map[Tup, F]] =
    val tup = tupleFromExprSmall(t)
    val tupf: Tuple.Map[tup.type, [T] =>> ReWrap[T, Expr, F]] = tup.map[[T] =>> ReWrap[T, Expr, F]]([T] => (et: T) => et.asInstanceOf[Expr[_]] match
      case '{ ($e: t) } => Expr.betaReduce('{ $f($e) }).asInstanceOf
    )
    Expr.ofTuple(tupf)
*/

  def mapUnrolled[Tup <: Tuple : Type, F[_] : Type](t: Expr[Tup], f: Expr[[X] => X => F[X]])(using Quotes): Expr[Tuple.Map[Tup, F]] =
    val bseq = untuple[Any](t)
    // BetaReduce doesn't work on poly functions yet!
    val rseq = bseq.map(arg => Expr.betaReduce('{ $f($arg) }))
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Tuple.Map[Tup, F]]]

  def mapBoundedUnrolled[Tup <: Tuple : Type, B : Type, R : Type](t: Expr[Tup], f: Expr[B => R])(using Quotes): Expr[Tuple.Map[Tup, [_] =>> R]] =
    val bseq = untuple[B](t)
    val rseq = bseq.map(arg => Expr.betaReduce(Expr.betaReduce(Expr.betaReduce('{ $f($arg) }))))
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Tuple.Map[Tup, [_] =>> R]]]

////  def mapPFCompiletime[Tup <: Tuple, R : Type](t: Expr[Tup], f: Expr[PartialFunction[Any, R]])(using Quotes): Expr[Tuple.Map[Tup, [_] =>> R]] =
////    val bseq = untuple[Any](t)
//
//    import quotes.reflect.*
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
//
//    println(evalMatch(DefDefBody(inlinedDefDef(f.asTerm))).asExprOf[Option[Any]])
//    println('{ $f(${bseq.last}) }.show)
//    println(Expr.betaReduce(Expr.betaReduce('{ $f(${bseq.last}) })).show)
//    val rseq = bseq.map(arg => arg.asTerm)
//    rseq.foreach(println)

    //    val bseq = constValueTuple[Tup]
//    val rseq = bseq.map(fv)
//    Expr(rseq)
//    ???


object ConstantArgsImpl:
  def forEachUnrolled(t: Expr[Seq[Any]], f: Expr[Any => Unit])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val Varargs(args) = t
    val exprs = args.map(arg => Expr.betaReduce('{ $f($arg) }))
    Expr.block(exprs.init.toList, exprs.last)

  def toTup(t: Expr[Seq[Any]])(using Quotes): Expr[Tuple] =
    import quotes.reflect.*
    val Varargs(args) = t
    Expr.ofTupleFromSeq(args)