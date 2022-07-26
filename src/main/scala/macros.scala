package macroloop.macros

import quoted.*
import scala.annotation.tailrec
import compiletime.*

def showImpl(e: Expr[Any])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(e.asTerm.show(using Printer.TreeShortCode))

def translateCodeImpl(x: Expr[Any], y: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*
  renameBoundFrom(x.asTerm, y.asTerm).asExpr

def simplifyTrivialInline(using q: Quotes)(x: q.reflect.Term, simplifyNamed: Boolean = false): q.reflect.Term =
  import quotes.reflect.*
  val rewr = new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case Inlined(None, Nil, b) => transformTerm(b)(owner)
      case Inlined(Some(Ident(_)), Nil, b) if simplifyNamed => transformTerm(b)(owner)
      case _ => super.transformTerm(tree)(owner)
  rewr.transformTerm(x)(Symbol.spliceOwner)

def gatherValDefSymbols(using q: Quotes)(x: q.reflect.Term): List[q.reflect.Symbol] =
  import collection.mutable.ListBuffer
  import quotes.reflect.*
  val acc = new TreeAccumulator[ListBuffer[Symbol]]:
    def foldTree(syms: ListBuffer[Symbol], tree: Tree)(owner: Symbol): ListBuffer[Symbol] = tree match
      case vdef @ ValDef(_, _, rhs) => rhs.map(t => foldTree(syms, t)(owner)).getOrElse(syms).append(vdef.symbol)
      case _ => foldOverTree(syms, tree)(owner)
  acc.foldTree(ListBuffer.empty, x)(Symbol.spliceOwner).result()

def translateRefs(using q: Quotes)(mapping: Map[q.reflect.Symbol, q.reflect.Symbol])(x: q.reflect.Term): q.reflect.Term =
  // TODO this can fail and should return an option
  import quotes.reflect.*
  val rewr = new TreeMap:
    override def transformStatement(tree: Statement)(owner: Symbol): Statement = tree match
      case d @ ValDef(_, _, rhs) => ValDef(mapping(d.symbol), rhs.map(transformTerm(_)(owner))): Statement
      case _ => super.transformStatement(tree)(owner)
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case i: Ident => mapping.unapply(i.symbol).fold(i)(s => Ident(s.termRef))
      case _ => super.transformTerm(tree)(owner)
  rewr.transformTerm(x)(Symbol.spliceOwner)

def renameBoundFrom(using q: Quotes)(toRename: q.reflect.Term, niceNames: q.reflect.Term): q.reflect.Term =
  val sourceNames = gatherValDefSymbols(toRename)
  val targetNames = gatherValDefSymbols(niceNames)
  // TODO this can fail and should flatMap into an option
  val mapping = (sourceNames zip targetNames).toMap
  translateRefs(mapping)(toRename)

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

object IterableItImpl:
  private def forEachE[T : Type](ito: Expr[IterableOnce[T]], ef: Expr[T] => Expr[Unit])(using Quotes): Expr[Unit] = '{
    val it: Iterator[T] = $ito.iterator
    while it.hasNext do
      val v = it.next()
      ${ ef('v) }
  }

  def forEach[T : Type](ite: Expr[IterableOnce[T]], f: Expr[T => Unit])(using Quotes): Expr[Unit] =
    forEachE(ite, et => '{ $f($et) })

  def forEachCart2[T1 : Type, T2 : Type](ite1: Expr[IterableOnce[T1]], ite2: Expr[Iterable[T2]], f: Expr[(T1, T2) => Unit])(using Quotes): Expr[Unit] =
    forEachE(ite1, et1 => forEachE(ite2, et2 => '{ $f($et1, $et2) }))

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
      while i < r do
        $f($a(i))
        i += 1
      while i < size do
        ${ foreachInRange(0, n)(j => '{ $f($a(i + ${Expr(j)})) }) }
        i += ${Expr(n)}
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