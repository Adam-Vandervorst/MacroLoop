package be.adamv.macroloop.macros

import scala.quoted.*
import be.adamv.macroloop.utils.*


object IntRangeImpl:
  def forEachUnrolled(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Unit])(using Quotes): Expr[Unit] =
    val range = Range(start.valueOrAbort, stop.valueOrAbort, step.valueOrAbort)
    val exprs = range.map(Expr(_)).map(i => betaReduceFixE('{ $f($i) }))
    Expr.block(exprs.init.toList, exprs.last)

  def forEach(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Unit])(using Quotes): Expr[Unit] =
    '{
      var i = $start
      while i < $stop do
        ${ betaReduceFixE('{ $f(i) }) }
        i += $step
    }

  def forall(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Boolean])(using Quotes): Expr[Boolean] =
    '{
      var i = $start
      while i < $stop do
        if ${ exprTreeTransform[Boolean](buildValDefElim)(betaReduceFixE('{ $f(i) })) } then
          i += $step
        else
          i = Int.MaxValue
      i < Int.MaxValue
    }

  def forallUnrolled(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Boolean])(using Quotes): Expr[Boolean] =
    val range = Range(start.valueOrAbort, stop.valueOrAbort, step.valueOrAbort)
    if range.isEmpty then Expr(true)
    else range.map(Expr(_)).map(i => betaReduceFixE('{ $f($i) })).reduce((x, y) => '{ $x && $y })

  def exists(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Boolean])(using Quotes): Expr[Boolean] =
    '{
      var i = $start
      while i < $stop do
        if ${ exprTreeTransform[Boolean](buildValDefElim)(betaReduceFixE('{ $f(i) })) } then
          i = Int.MaxValue
        else
          i += $step
      i == Int.MaxValue
    }

  def existsUnrolled(start: Expr[Int], stop: Expr[Int], step: Expr[Int], f: Expr[Int => Boolean])(using Quotes): Expr[Boolean] =
    val range = Range(start.valueOrAbort, stop.valueOrAbort, step.valueOrAbort)
    if range.isEmpty then Expr(false)
    else range.map(Expr(_)).map(i => betaReduceFixE('{ $f($i) })).reduce((x, y) => '{ $x || $y })

  def forEachZipped2(sss1: Expr[(Int, Int, Int)], sss2: Expr[(Int, Int, Int)], f: Expr[(Int, Int) => Unit])(using Quotes): Expr[Unit] =
    val Seq(start1, stop1, step1) = untuple[Int](sss1)
    val Seq(start2, stop2, step2) = untuple[Int](sss2)

    '{
      var i1 = $start1
      var i2 = $start2

      while i1 < $stop1 && i2 < $stop2 do
        ${ exprTreeTransform[Unit](buildValDefElim)(betaReduceFixE('{ $f(i1, i2) })) }
        i1 += $step1
        i2 += $step2
    }

  def forEachZipped[N <: Int : Type](ssst: Expr[Repeat[N, (Int, Int, Int)]], f: Expr[Repeat[N, Int] => Unit])(using q: Quotes): Expr[Unit] =
    val Seq(starts, stops, steps) = untuple[(Int, Int, Int)](ssst).map(untuple[Int](_)).transpose

    def rec(c: Int, updates: List[Expr[Unit]], conditions: List[Expr[Boolean]], args: Tuple): Expr[Unit] =
      if c < 0 then '{
        while ${ conditions.reduce((x, y) => '{ $x && $y }) } do
          ${ applyTupleDestruct[Repeat[N, Int], Unit](args.asInstanceOf, f) }
          ${ Expr.block(updates.init, updates.last) }
      } else starts(c) mutating { (value, update) =>
        rec(c - 1,
          exprTreeTransform[Unit](buildValDefElim)(betaReduceFixE('{ $update($value + ${ steps(c) }) }))::updates,
          '{ $value < ${ stops(c) } }::conditions,
          value *: args)
      }

    rec(starts.length - 1, Nil, Nil, EmptyTuple)
