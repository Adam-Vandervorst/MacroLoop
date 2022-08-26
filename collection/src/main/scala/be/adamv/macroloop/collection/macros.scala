package be.adamv.macroloop.collection.macros

import be.adamv.macroloop.macros.{untuple, SizedArrayIndexImpl}
import be.adamv.macroloop.collection.{Matrix, SizedVector}

import scala.quoted.*


def concreteMatrixImpl[M <: Int : Type, N <: Int : Type, A : Type](rowse: Expr[Tuple])(using Quotes): Expr[Matrix[M, N, A]] =
  val rows = untuple[Tuple](rowse)
  val nrows = rows.length
  val elements: Seq[Seq[Expr[A]]] = rows.map(untuple[A])
  val ncolumns = elements.head.length
  assert(elements.forall(_.length == ncolumns))

  '{
  val ar = ${ SizedArrayIndexImpl.ofSizeImpl[A](Expr(nrows*ncolumns)) }
    ${
    Expr.block(elements.zipWithIndex.flatMap((row: Seq[Expr[A]], i: Int) => 
      row.zipWithIndex.map((e, j) => 
        '{ ar(${ Expr(i*ncolumns + j) }) = $e })).toList,
      '{ new Matrix[M, N, A]{ override val data: Array[A] = ar } })
    }

  }


def concreteVectorImpl[N <: Int : Type, A : Type](elementse: Expr[Tuple])(using Quotes): Expr[SizedVector[N, A]] =
  val elements = untuple[A](elementse)
  val size = elements.length
  '{
    val ar = ${ SizedArrayIndexImpl.ofSizeImpl[A](Expr(size)) }
    ${
      Expr.block(elements.zipWithIndex.map((m, i) => '{ ar(${ Expr(i) }) = $m }).toList,
        '{ new SizedVector[N, A]{ override val data: Array[A] = ar } })
    }
  }