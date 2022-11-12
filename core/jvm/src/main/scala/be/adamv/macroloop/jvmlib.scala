package be.adamv.macroloop

import be.adamv.macroloop.macros.*


inline def staticClass[A]: Class[A] = ${ staticClassImpl[A] }

object SizedArrayIndex:
  inline def ofSize[S <: Int, A] = ${ SizedArrayIndexImpl.ofSizeTypeImpl[S, A] }
  inline def ofSize[A](inline s: Int) = ${ SizedArrayIndexImpl.ofSizeImpl[A]('s) }

  inline def mapForSize[T, R](inline a: Array[T], inline n: Int)(inline f: T => R): Array[R] =
    inline if n <= 16 then
    mapUnrolled(a, n)(f)
    else if n > 1000000 then
      mapUnrolledN(32)(a, n)(f)
    else
      map(a, n)(f)

  inline def map[T, R](inline a: Array[T], inline n: Int)(inline f: T => R): Array[R] =
    ${ SizedArrayIndexImpl.map('a, 'f, 'n) }

  inline def mapUnrolled[T, R](inline a: Array[T], inline n: Int)(inline f: T => R): Array[R] =
    ${ SizedArrayIndexImpl.mapUnrolled('a, 'f, 'n) }

  inline def mapUnrolledN[T, R](inline k: Int)(inline a: Array[T], inline n: Int)(inline f: T => R): Array[R] =
    ${ SizedArrayIndexImpl.mapUnrolledN('a, 'f, 'n, 'k) }

  inline def flatMapFullyUnrolled[T, R](inline a: Array[T], inline n: Int)(inline f: T => Array[R], inline m: Int): Array[R] =
    ${ SizedArrayIndexImpl.flatMapFullyUnrolled('a, 'f, 'n, 'm) }

object ArrayIndex:
  inline def forEach[T](inline a: Array[T])(inline f: T => Unit): Unit =
    ${ ArrayIndexImpl.forEach('a, 'f) }

  inline def map[T, R](inline a: Array[T])(inline f: T => R): Array[R] =
    ${ ArrayIndexImpl.map('a, 'f) }

  inline def forEachUnrolledN[T](inline n: Int)(inline a: Array[T])(inline f: T => Unit): Unit =
    ${ ArrayIndexImpl.forEachUnrolledN('a, 'f, 'n) }

  inline def forallException[T](inline a: Array[T])(inline f: T => Boolean): Boolean =
    ${ ArrayIndexImpl.forallException('a, 'f) }

  inline def forallCondition[T](inline a: Array[T])(inline f: T => Boolean): Boolean =
    ${ ArrayIndexImpl.forallCondition('a, 'f) }
