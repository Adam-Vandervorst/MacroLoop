// TODO these macros have no tests yet; how to reuse `LiteralFunSuite`?
/**
 * Macros intrinsically bound to the collections.
 * Currently tuple and tuple of tuples to sized vector and matrix respectively.
 */
package be.adamv.macroloop.collection.macros

import be.adamv.macroloop.macros.{untuple, SizedArrayIndexImpl}
import be.adamv.macroloop.collection.{ArraySizedVector}

import scala.quoted.*

/**
 * Takes desired matrix dimensions and converts a nested tuple literal to a flat array with these contents (wrapped by Matrix).
 */
//def concreteMatrixImpl[M <: Int : Type, N <: Int : Type, A : Type](rowse: Expr[Tuple])(using Quotes): Expr[Matrix[M, N, A]] =
//  // FIXME may miss some edge cases
//  val rows = untuple[Tuple](rowse)
//  val nrows = rows.length
//  val elements: Seq[Seq[Expr[A]]] = rows.map(untuple[A])
//  val ncolumns = elements.head.length
//
//  // FIXME we don't actually *need* M and N to correspond to nrows and ncolumns, are there any good usecases where these checks fail?
//  // TODO throw decent errors
//  assert(Type.valueOfConstant[M].get == nrows)
//  assert(Type.valueOfConstant[N].get == ncolumns)
//  assert(elements.forall(_.length == ncolumns))
//
//  // TODO Special case empty tuple (of tuples)?
//  // TODO Speed up using Array literals? Do they nest?
//  '{
//  val ar = ${ SizedArrayIndexImpl.ofSizeImpl[A](Expr(nrows*ncolumns)) }
//  ${
//  Expr.block(elements.zipWithIndex.flatMap((row: Seq[Expr[A]], i: Int) =>
//    row.zipWithIndex.map((e, j) =>
//      '{ ar(${ Expr(i*ncolumns + j) }) = $e })).toList,
//    '{ new Matrix[M, N, A]{ override val data: Array[A] = ar } })
//  }
//  }

/**
 * Takes desired vector length and converts a tuple literal to an array with the tuple contents (wrapped by SizedVector).
 */
def concreteVectorImpl[N <: Int : Type, A : Type](elementse: Expr[Tuple])(using Quotes): Expr[ArraySizedVector[N, A]] =
  val elements = untuple[A](elementse)
  val size = elements.length

  // FIXME we don't actually *need* N to correspond to size, are there any good usecases where this check fails?
  // TODO throw a decent error
  assert(Type.valueOfConstant[N].get == size)

  // TODO Special case empty tuple?
  // TODO Speed up using Array literals?
  '{
  val ar = ${ SizedArrayIndexImpl.ofSizeImpl[A](Expr(size)) }
  ${
  Expr.block(elements.zipWithIndex.map((m, i) =>
    '{ ar(${ Expr(i) }) = $m }).toList,
    '{ new ArraySizedVector[N, A]{ override val data: Array[A] = ar } })
  }
  }