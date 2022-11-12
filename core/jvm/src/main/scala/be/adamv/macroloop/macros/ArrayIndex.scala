package be.adamv.macroloop.macros

import scala.quoted.*
import scala.annotation.tailrec


object ArrayIndexImpl:
  def forEach[T : Type](a: Expr[Array[T]], f: Expr[T => Unit])(using Quotes): Expr[Unit] =
    '{
      val size = $a.length
      var i = 0
      while i < size do
        ${ betaReduceFixE('{ $f($a(i)) }) }
        i += 1
    }

  def map[T : Type, R : Type](a: Expr[Array[T]], f: Expr[T => R])(using Quotes): Expr[Array[R]] =
    '{
      val size = $a.length
      val na = ${ SizedArrayIndexImpl.ofSizeImpl[R]('{ size }) }
      var i = 0
      while i < size do
        ${ exprTreeTransform[Unit](buildValDefElim)('{ na(i) = ${ betaReduceFixE('{ $f($a(i)) }) } }) }
        i += 1
      na
    }

  def forEachUnrolledN[T : Type](a: Expr[Array[T]], f: Expr[T => Unit], ne: Expr[Int])(using Quotes): Expr[Unit] =
    val n = ne.valueOrAbort
    '{
      val size = $a.length
      val r = size % ${Expr(n)}
      var i = 0
      while i < r do
        ${ betaReduceFixE('{ $f($a(i)) }) }
        i += 1
      while i < size do
        ${ betaReduceFixE('{ $f($a(i)) }) }
        ${ foreachInRange(1, n)(j => betaReduceFixE('{ $f($a(i + ${Expr(j)})) })) }
        i += ${Expr(n)}
    }

  def forallException[T : Type](a: Expr[Array[T]], f: Expr[T => Boolean])(using Quotes): Expr[Boolean] =
    '{
      try
        val size = $a.length
        var i = 0
        while i < size do
          if ${ betaReduceFixE('{ $f($a(i)) })} then i += 1
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
        if ${ betaReduceFixE('{ $f($a(i)) })} then i += 1
        else b = false
      b
    }

  def foreachInRange(start: Int, end: Int)(f: Int => Expr[Unit])(using Quotes): Expr[Unit] =
    @tailrec def unroll(i: Int, acc: Expr[Unit]): Expr[Unit] =
      if (i < end) unroll(i + 1, '{ $acc; ${f(i)} }) else acc
    if (start < end) unroll(start + 1, f(start)) else '{}
