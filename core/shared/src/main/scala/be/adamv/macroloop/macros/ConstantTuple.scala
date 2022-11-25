package be.adamv.macroloop.macros

import scala.quoted.*
import be.adamv.macroloop.utils.*

object ConstantTupleImpl:
  def prepend[Tup <: Tuple : Type, X: Type](te: Expr[Tup], xe: Expr[X])(using Quotes): Expr[X *: Tup] =
    val tseq = untuple[Any](te)
    tupleToExpr[X *: Tup](xe::tseq).asInstanceOf[Expr[X *: Tup]]

  def append[Tup <: Tuple : Type, X: Type](te: Expr[Tup], xe: Expr[X])(using Quotes): Expr[Tuple.Append[Tup, X]] =
    val tseq = untuple[Any](te)
    Expr.ofTupleFromSeq(tseq.appended(xe)).asInstanceOf[Expr[Tuple.Append[Tup, X]]]

  def insert[Tup <: Tuple : Type, X: Type](te: Expr[Tup], pose: Expr[Int], xe: Expr[X])(using Quotes): Expr[Tuple] =
    val pos = pose.valueOrAbort
    val tbuf = untuple[Any](te).toBuffer
    tbuf.insert(pos, xe)
    Expr.ofTupleFromSeq(tbuf.toSeq)

  def concat[T1 <: Tuple : Type, T2 <: Tuple : Type](t1e: Expr[T1], t2e: Expr[T2])(using Quotes): Expr[Tuple.Concat[T1, T2]] =
    val t1seq = untuple[Any](t1e)
    val t2seq = untuple[Any](t2e)
    Expr.ofTupleFromSeq(t1seq ++ t2seq).asInstanceOf[Expr[Tuple.Concat[T1, T2]]]

  def forEachUnrolled[Tup <: Tuple : Type](t: Expr[Tup], f: Expr[Any => Unit])(using Quotes): Expr[Unit] =
    val bseq = untuple[Any](t)
    val exprs = bseq.map(arg => exprTreeTransform[Unit](buildValDefElim)(betaReduceFixE('{ $f($arg) })))
    Expr.block(exprs.init.toList, exprs.last)

  def forEachBoundedUnrolled[Tup <: Tuple : Type, B: Type](t: Expr[Tup], f: Expr[B => Unit])(using Quotes): Expr[Unit] =
    val bseq = untuple[B](t)
    val exprs = bseq.map(arg => exprTreeTransform[Unit](buildValDefElim)(betaReduceFixE('{ $f($arg) })))
    Expr.block(exprs.init.toList, exprs.last)

  def mapUnrolled[Tup <: Tuple : Type, F[_] : Type](t: Expr[Tup], f: Expr[[X] => X => F[X]])(using Quotes): Expr[Tuple.Map[Tup, F]] =
    val bseq = untuple[Any](t)
    // BetaReduce doesn't work on poly functions yet!
    val rseq = bseq.map(arg => '{$f($arg)})
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Tuple.Map[Tup, F]]]

  def mapFlatUnrolled[N <: Int, B: Type, R: Type](t: Expr[Repeat[N, B]], f: Expr[B => R])(using Quotes): Expr[Repeat[N, R]] =
    val bseq = untuple[B](t)
    val rseq = bseq.map(arg => exprTreeTransform[R](buildValDefElim)(betaReduceFixE('{ $f($arg) })))
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Repeat[N, R]]]

  def mapBoundedUnrolled[Tup <: Tuple : Type, B: Type, R: Type](t: Expr[Tup], f: Expr[B => R])(using Quotes): Expr[Tuple.Map[Tup, [_] =>> R]] =
    val bseq = untuple[B](t)
    val rseq = bseq.map(arg => exprTreeTransform[R](buildValDefElim)(betaReduceFixE('{ $f($arg) })))
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Tuple.Map[Tup, [_] =>> R]]]

  def tabulateUnrolled[N <: Int : Type, R: Type](ne: Expr[N], f: Expr[Int => R])(using Quotes): Expr[Repeat[N, R]] =
    val length = ne.valueOrAbort
    val rseq = Seq.range(0, length).map(i => exprTreeTransform[R](buildValDefElim)(betaReduceFixE('{ $f(${ Expr(i) }) })))
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Repeat[N, R]]]

  def fillUnrolled[N <: Int : Type, R: Type](ne: Expr[N], f: Expr[R])(using Quotes): Expr[Repeat[N, R]] =
    val length = ne.valueOrAbort
    val rseq = Seq.range(0, length).map(_ => exprTreeTransform[R](buildValDefElim)(betaReduceFixE(f)))
    Expr.ofTupleFromSeq(rseq).asInstanceOf[Expr[Repeat[N, R]]]
