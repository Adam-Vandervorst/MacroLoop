package be.adamv.macroloop

import be.adamv.macroloop.macros.*
import be.adamv.macroloop.utils.Repeat

import scala.annotation.targetName


inline def show(inline a: Any): String = ${ showImpl('a) }
inline def showTree(inline a: Any): String = ${ showTreeImpl('a) }
transparent inline def stripCast[T](inline t: T): T = ${ stripCastImpl('t) }
// transparent prevents tagging the returned code with an extra typing judgement
transparent inline def translateCode(inline x: Any, inline y: Any): Any = ${ translateCodeImpl('x, 'y) }


object IntRange:
  inline def forEach(inline stop: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEach('{ 0 }, 'stop, '{ 1 }, 'f) }
  inline def forEach(inline start: Int, inline stop: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEach('start, 'stop, '{ 1 }, 'f) }
  inline def forEach(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEach('start, 'stop, 'step, 'f) }

  inline def forEachUnrolled(inline stop: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEachUnrolled('{ 0 }, 'stop, '{ 1 }, 'f) }
  inline def forEachUnrolled(inline start: Int, inline stop: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEachUnrolled('start, 'stop, '{ 1 }, 'f) }
  inline def forEachUnrolled(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Unit): Unit =
    ${ IntRangeImpl.forEachUnrolled('start, 'stop, 'step, 'f) }

  inline def forall(inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.forall('{ 0 }, 'stop, '{ 1 }, 'f) }
  inline def forall(inline start: Int, inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.forall('start, 'stop, '{ 1 }, 'f) }
  inline def forall(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.forall('start, 'stop, 'step, 'f) }

  inline def forallUnrolled(inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${IntRangeImpl.forallUnrolled('{ 0 }, 'stop, '{ 1 }, 'f)}
  inline def forallUnrolled(inline start: Int, inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${IntRangeImpl.forallUnrolled('start, 'stop, '{ 1 }, 'f)}
  inline def forallUnrolled(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.forallUnrolled('start, 'stop, 'step, 'f) }

  inline def exists(inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.exists('{ 0 }, 'stop, '{ 1 }, 'f) }
  inline def exists(inline start: Int, inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${IntRangeImpl.exists('start, 'stop, '{ 1 }, 'f)}
  inline def exists(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.exists('start, 'stop, 'step, 'f) }

  inline def existsUnrolled(inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.existsUnrolled('{ 0 }, 'stop, '{ 1 }, 'f) }
  inline def existsUnrolled(inline start: Int, inline stop: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.existsUnrolled('start, 'stop, '{ 1 }, 'f) }
  inline def existsUnrolled(inline start: Int, inline stop: Int, inline step: Int)(inline f: Int => Boolean): Boolean =
    ${ IntRangeImpl.existsUnrolled('start, 'stop, 'step, 'f) }

  inline def forEachZipped2(inline s1: Int, inline s2: Int)(inline f: (Int, Int) => Unit): Unit =
    ${ IntRangeImpl.forEachZipped2('{ (0, s1, 1) }, '{ (0, s2, 1) }, 'f) }
  inline def forEachZipped2(inline ss1: (Int, Int), inline ss2: (Int, Int))(inline f: (Int, Int) => Unit): Unit =
    ${ IntRangeImpl.forEachZipped2('{ ConstantTuple.append(ss1)(1) }, '{ ConstantTuple.append(ss2)(1) }, 'f) }
  inline def forEachZipped2(inline sss1: (Int, Int, Int), inline sss2: (Int, Int, Int))(inline f: (Int, Int) => Unit): Unit =
    ${ IntRangeImpl.forEachZipped2('sss1, 'sss2, 'f) }

//  @targetName("forEachZippedAbbreviated")
//  inline def forEachZipped[N <: Int](inline sst: Repeat[N, (Int, Int)])(inline f: Repeat[N, Int] => Unit): Unit =
//    ${ IntRangeImpl.forEachZipped[N]('{ ConstantTuple.mapFlatUnrolled(sst)((a: (Int, Int)) => ConstantTuple.append(a)(1)) }, 'f) }
  inline def forEachZipped[N <: Int](inline ssst: Repeat[N, (Int, Int, Int)])(inline f: Repeat[N, Int] => Unit): Unit =
    ${ IntRangeImpl.forEachZipped('ssst, 'f) }

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

object ConstantList:
  transparent inline def toTuple22(inline l: List[Any]): Tuple =
    ${ ConstantListImpl.toTuple22('l) }

object ConstantTuple:
  import compiletime.{constValue, erasedValue}
  transparent inline def constToList[Tup <: Tuple]: List[Any] = inline erasedValue[Tup] match
    case _: EmptyTuple => Nil
    case _: (head *: tail) => constValue[head] :: constToList[tail]

  transparent inline def prepend[Tup <: Tuple, X](inline t: Tup)(inline x: X): X *: Tup =
    ${ ConstantTupleImpl.prepend('t, 'x) }

  transparent inline def append[Tup <: Tuple, X](inline t: Tup)(inline x: X): Tuple.Append[Tup, X] =
    ${ ConstantTupleImpl.append('t, 'x) }

  transparent inline def insert[Tup <: Tuple, I <: Int, X](inline t: Tup)(inline pos: I, inline x: X): Tuple =
    ${ ConstantTupleImpl.insert('t, 'pos, 'x) }

  transparent inline infix def concat[T1 <: Tuple, T2 <: Tuple](inline t1: T1, inline t2: T2): Tuple.Concat[T1, T2] =
    ${ ConstantTupleImpl.concat('t1, 't2) }

  inline def forEachUnrolled[Tup <: Tuple](inline t: Tup)(inline f: Any => Unit): Unit =
    ${ ConstantTupleImpl.forEachUnrolled('t, 'f) }

  inline def forEachBoundedUnrolled[Tup <: Tuple, B](inline t: Tup)(inline f: B => Unit): Unit =
    ${ ConstantTupleImpl.forEachBoundedUnrolled('t, 'f) }

  inline def mapUnrolled[Tup <: Tuple, F[_]](inline t: Tup)(inline f: [X] => X => F[X]): Tuple.Map[Tup, F] =
    ${ ConstantTupleImpl.mapUnrolled[Tup, F]('t, 'f) }

  inline def mapFlatUnrolled[N <: Int, B, R](inline t: Repeat[N, B])(inline f: B => R): Repeat[N, R] =
    ${ ConstantTupleImpl.mapFlatUnrolled('t, 'f) }

  inline def mapBoundedUnrolled[Tup <: Tuple, B, R](inline t: Tup)(inline f: B => R): Tuple.Map[Tup, [_] =>> R] =
    ${ ConstantTupleImpl.mapBoundedUnrolled('t, 'f) }

  transparent inline def tabulateUnrolled[N <: Int, R](inline n: N)(inline f: Int => R): Repeat[N, R] =
    ${ConstantTupleImpl.tabulateUnrolled('n, 'f)}

  transparent inline def fillUnrolled[N <: Int, R](inline n: N)(inline f: => R): Repeat[N, R] =
    ${ConstantTupleImpl.fillUnrolled('n, 'f)}


object ConstantArgs:
  inline def forEachUnrolled(inline args: Any*)(inline f: Any => Unit): Unit =
    ${ ConstantArgsImpl.forEachUnrolled('args, 'f) }

  transparent inline def toTup(inline args: Any*) =
    ${ ConstantArgsImpl.toTup('args) }

