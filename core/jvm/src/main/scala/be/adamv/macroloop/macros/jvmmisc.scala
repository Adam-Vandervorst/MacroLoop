package be.adamv.macroloop.macros

import scala.quoted.*
import scala.reflect.ClassTag


def staticClassImpl[A : Type](using Quotes): Expr[Class[A]] =
  import quotes.reflect.*

  Implicits.search(TypeRepr.of[Class].appliedTo(TypeRepr.of[A])) match
    case s: ImplicitSearchSuccess =>
      s.tree.asExprOf[Class[A]]
    case _ =>
      report.errorAndAbort(f"${TypeRepr.of[A].show} is not a class")
