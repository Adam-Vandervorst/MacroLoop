package be.adamv.macroloop.macros

import scala.quoted.{Expr, Quotes}

object ConstantListImpl:
  def toTuple22(l: Expr[List[Any]])(using Quotes): Expr[Tuple] =
    Expr.ofTupleFromSeq(unlist(l))
