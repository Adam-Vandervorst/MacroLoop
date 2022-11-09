package be.adamv.macroloop.macros

import scala.compiletime.ops.int.S
import scala.quoted.*


object IterableItImpl:
  private def forEachE[T : Type](ito: Expr[IterableOnce[T]], ef: Expr[T] => Expr[Unit])(using Quotes): Expr[Unit] = '{
    val it: Iterator[T] = $ito.iterator
    while it.hasNext do
      val v = it.next()
      ${ betaReduceFixE(ef('v)) }
  }

  def forEach[T : Type](ite: Expr[IterableOnce[T]], f: Expr[T => Unit])(using Quotes): Expr[Unit] =
    forEachE(ite, et => '{ $f($et) })

  def forEachCart2[T1 : Type, T2 : Type](ite1: Expr[IterableOnce[T1]], ite2: Expr[Iterable[T2]], f: Expr[(T1, T2) => Unit])(using Quotes): Expr[Unit] =
    forEachE(ite1, et1 => forEachE(ite2, et2 => '{ $f($et1, $et2) }))

  def forEachCart[Tup <: Tuple : Type](tite: Expr[Tuple.Map[Tup, Iterable]], f: Expr[Tup => Unit])(using Quotes): Expr[Unit] =
    val seq = untuple[Iterable[Any]](tite)
    def unroll[I <: Int : Type](ites: Seq[Expr[Iterable[Any]]], args: Tuple): Expr[Unit] = ites match
      case Nil => '{ $f(${ tupleToExpr[Tup](args.asInstanceOf) }) }
      case ite::ites => forEachE(ite.asExprOf[Iterable[Tuple.Elem[Tup, I]]], et => unroll[S[I]](ites, args :* et))
    unroll[0](seq, EmptyTuple)

  def forallExceptionCart[Tup <: Tuple : Type](tite: Expr[Tuple.Map[Tup, Iterable]], f: Expr[Tup => Boolean])(using q: Quotes): Expr[Boolean] =
    val seq = untuple[Iterable[Any]](tite)
    def unroll[I <: Int : Type](ites: Seq[Expr[Iterable[Any]]], args: Tuple): Expr[Unit] = ites match
      case Nil => '{ if !${ applyTupleDestruct[Tup, Boolean](args.asInstanceOf, f) } then throw Break }
      case ite::ites => forEachE(ite.asExprOf[Iterable[Tuple.Elem[Tup, I]]], et => unroll[S[I]](ites, args :* et))
    '{
      try
        ${ unroll[0](seq, EmptyTuple) }
        true
      catch
        case Break => false
    }
