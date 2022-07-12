package macroloop

import macros.*

inline def show(inline a: Any): String = ${ showImpl('a) }

object IntRange:
  inline def forEach(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEach('start, 'stop, 'step, 'f) }

  inline def forEachUnrolled(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEachUnrolled('start, 'stop, 'step, 'f) }

object ArrayIndex:
  inline def forEach[T](inline a: Array[T])(inline f: T => Unit): Unit =
    ${ ArrayIndexImpl.forEach('a, 'f) }

  inline def forEachUnrolledN[T, N <: Int & Singleton](inline n: N)(inline a: Array[T])(inline f: T => Unit): Unit =
    ${ ArrayIndexImpl.forEachUnrolledN('a, 'f, 'n) }

//  inline def forEachUnrolled16[T](inline a: Array[T])(inline f: T => Unit): Unit =
//    ${ ArrayIndexImpl.forEachUnrolledN('a, 'f, 16) }

  inline def forallException[T](inline a: Array[T])(inline f: T => Boolean): Boolean =
    ${ ArrayIndexImpl.forallException('a, 'f) }

  inline def forallCondition[T](inline a: Array[T])(inline f: T => Boolean): Boolean =
    ${ ArrayIndexImpl.forallCondition('a, 'f) }

import macroloop.staging

object ConstantTuple:
  inline def forEachUnrolled[Tup <: Tuple](inline t: Tup)(inline f: Any => Unit): Unit =
    ${ ConstantTupleImpl.forEachUnrolled('t, 'f) }

  inline def mapUnrolled[Tup <: Tuple, F[_]](inline t: Tup)(inline f: [X] => X => F[X]): Tuple.Map[Tup, F] =
    ${ ConstantTupleImpl.mapUnrolled[Tup, F]('t, 'f) }

  inline def mapBoundedUnrolled[Tup <: Tuple, B, R](inline t: Tup)(inline f: B => R): Tuple.Map[Tup, [_] =>> R] =
    ${ ConstantTupleImpl.mapBoundedUnrolled('t, 'f) }

  inline def mapPFCompiletime[Tup <: NonEmptyTuple, R](inline t: Tup)(inline f: PartialFunction[Any, R]): Tuple.Map[Tup, [_] =>> R] =
    ${ staging.ConstantTuple.mapPF('t, 'f) }


object ConstantArgs:
  inline def forEachUnrolled(inline args: Any*)(inline f: Any => Unit): Unit =
    ${ ConstantArgsImpl.forEachUnrolled('args, 'f) }

  transparent inline def toTup(inline args: Any*) =
    ${ ConstantArgsImpl.toTup('args) }

