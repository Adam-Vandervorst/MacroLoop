package be.adamv.macroloop.macros

import scala.quoted.*

object ConstantArgsImpl:
  def forEachUnrolled(t: Expr[Seq[Any]], f: Expr[Any => Unit])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val Varargs(args) = t: @unchecked
    val exprs = args.map(arg => exprTreeTransform[Unit](buildValDefElim)(betaReduceFixE('{ $f($arg) })))
    Expr.block(exprs.init.toList, exprs.last)

  def toTup(t: Expr[Seq[Any]])(using Quotes): Expr[Tuple] =
    import quotes.reflect.*
    val Varargs(args) = t: @unchecked
    Expr.ofTupleFromSeq(args)