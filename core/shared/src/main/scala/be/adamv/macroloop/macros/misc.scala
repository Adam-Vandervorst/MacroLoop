package be.adamv.macroloop.macros

import scala.quoted.*

def showImpl(e: Expr[Any])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(e.asTerm.show(using Printer.TreeShortCode))

def showTreeImpl(e: Expr[Any])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(e.asTerm.show(using Printer.TreeStructure))

def stripCast(using q: Quotes): q.reflect.Term => q.reflect.Term =
  import q.reflect.*
  def rec(tree: Term): Term = tree match
    case Inlined(_, Nil, e) => rec(e)
    case TypeApply(Select(x, "asInstanceOf" | "$asInstanceOf$"), _) => rec(x)
    case x => x
  rec

def stripCastImpl[T : Type](e: Expr[T])(using Quotes): Expr[T] =
  exprTransform[T](stripCast)(e)

def translateCodeImpl(x: Expr[Any], y: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*
  renameBoundFrom(x.asTerm, y.asTerm).asExpr
