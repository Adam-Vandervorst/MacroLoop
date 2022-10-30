package be.adamv.macroloop.utils

import scala.annotation.tailrec
import scala.compiletime.ops.int.S

@tailrec
def fix[A](f: A => A)(v: A, prev: Option[A] = None): A =
  if (prev.isEmpty || v != prev.get) fix(f)(f(v), Some(v))
  else v

type Const = Boolean | Byte | Short | Int | Long | Float | Double | Char | String


extension [X](xs: Iterable[X])
  def occurrences: Iterable[Int] =
    xs.map(s => xs.iterator.indexOf(s))


type Repeat[N <: Int, A] <: Tuple = N match
  case 0 => EmptyTuple
  case S[n] => A *: Repeat[n, A]