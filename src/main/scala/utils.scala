package macroloop.utils

import scala.annotation.tailrec

@tailrec
def fix[A](f: A => A)(v: A, prev: Option[A] = None): A =
  if (prev.isEmpty || v != prev.get) fix(f)(f(v), Some(v))
  else v