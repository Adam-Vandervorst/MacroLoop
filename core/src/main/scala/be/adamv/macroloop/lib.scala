package be.adamv.macroloop

import be.adamv.macroloop.macros.*


inline def show(inline a: Any): String = ${ showImpl('a) }
inline def showTree(inline a: Any): String = ${ showTreeImpl('a) }
transparent inline def stripCast[T](inline t: T): T = ${ stripCastImpl('t) }
// transparent prevents tagging the returned code with an extra typing judgement
transparent inline def translateCode(inline x: Any, inline y: Any): Any = ${ translateCodeImpl('x, 'y) }
inline def staticClass[A]: Class[A] = ${ staticClassImpl[A] }


object IntRange:
  inline def forEach(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEach('start, 'stop, 'step, 'f) }

  inline def forEachUnrolled(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEachUnrolled('start, 'stop, 'step, 'f) }

object IterableIt:
  // `Iterator` arguments are *not* inlined
  inline def forEach[T](it: IterableOnce[T])(inline f: T => Unit): Unit =
    ${ IterableItImpl.forEach('it, 'f) }

  inline def forEachCart2[T1, T2](it1: IterableOnce[T1], it2: Iterable[T2])(inline f: (T1, T2) => Unit): Unit =
    ${ IterableItImpl.forEachCart2('it1, 'it2, 'f) }

  inline def forEachCart[Tup <: Tuple](inline tite: Tuple.Map[Tup, Iterable])(inline f: Tup => Unit): Unit =
    ${ IterableItImpl.forEachCart('tite, 'f) }

  inline def forallExceptionCart[Tup <: Tuple](inline tite: Tuple.Map[Tup, Iterable])(inline f: Tup => Boolean): Boolean =
    ${ IterableItImpl.forallExceptionCart('tite, 'f) }

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

  inline def mapUnrolledN[T, R, N <: Int & Singleton](inline k: N)(inline a: Array[T], inline n: Int)(inline f: T => R): Array[R] =
    ${ SizedArrayIndexImpl.mapUnrolledN('a, 'f, 'n, 'k) }

  inline def flatMapFullyUnrolled[T, R](inline a: Array[T], inline n: Int)(inline f: T => Array[R], inline m: Int): Array[R] =
    ${ SizedArrayIndexImpl.flatMapFullyUnrolled('a, 'f, 'n, 'm) }

object ArrayIndex:
  inline def forEach[T](inline a: Array[T])(inline f: T => Unit): Unit =
    ${ ArrayIndexImpl.forEach('a, 'f) }

  inline def map[T, R](inline a: Array[T])(inline f: T => R): Array[R] =
    ${ ArrayIndexImpl.map('a, 'f) }

  inline def forEachUnrolledN[T, N <: Int & Singleton](inline n: N)(inline a: Array[T])(inline f: T => Unit): Unit =
    ${ ArrayIndexImpl.forEachUnrolledN('a, 'f, 'n) }

  inline def forallException[T](inline a: Array[T])(inline f: T => Boolean): Boolean =
    ${ ArrayIndexImpl.forallException('a, 'f) }

  inline def forallCondition[T](inline a: Array[T])(inline f: T => Boolean): Boolean =
    ${ ArrayIndexImpl.forallCondition('a, 'f) }

object ConstantList:
  transparent inline def toTuple22(inline l: List[Any]): Tuple =
    ${ ConstantListImpl.toTuple22('l) }

object ConstantTuple:
  import compiletime.{constValue, erasedValue}
  transparent inline def constToList[Tup <: Tuple]: List[Any] = inline erasedValue[Tup] match
    case _: EmptyTuple => Nil
    case _: (head *: tail) => constValue[head] :: constToList[tail]

  inline def forEachUnrolled[Tup <: Tuple](inline t: Tup)(inline f: Any => Unit): Unit =
    ${ ConstantTupleImpl.forEachUnrolled('t, 'f) }

  inline def forEachBoundedUnrolled[Tup <: Tuple, B](inline t: Tup)(inline f: B => Unit): Unit =
    ${ ConstantTupleImpl.forEachBoundedUnrolled('t, 'f) }

  inline def mapUnrolled[Tup <: Tuple, F[_]](inline t: Tup)(inline f: [X] => X => F[X]): Tuple.Map[Tup, F] =
    ${ ConstantTupleImpl.mapUnrolled[Tup, F]('t, 'f) }

  inline def mapBoundedUnrolled[Tup <: Tuple, B, R](inline t: Tup)(inline f: B => R): Tuple.Map[Tup, [_] =>> R] =
    ${ ConstantTupleImpl.mapBoundedUnrolled('t, 'f) }

//  inline def mapPFCompiletime[Tup <: NonEmptyTuple, R](inline t: Tup)(inline f: PartialFunction[Any, R]): Tuple.Map[Tup, [_] =>> R] =
//    ${ staging.ConstantTuple.mapPF('t, 'f) }


object ConstantArgs:
  inline def forEachUnrolled(inline args: Any*)(inline f: Any => Unit): Unit =
    ${ ConstantArgsImpl.forEachUnrolled('args, 'f) }

  transparent inline def toTup(inline args: Any*) =
    ${ ConstantArgsImpl.toTup('args) }

