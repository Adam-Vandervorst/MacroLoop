package be.adamv.macroloop.macros

object SizedArrayIndexImpl:
  def ofSizeTypeImpl[S <: Int : Type, A : Type](using Quotes): Expr[Array[A]] =
    ofSizeImpl(Expr(Type.valueOfConstant[S].get))

  def ofSizeImpl[A : Type](size: Expr[Int])(using Quotes): Expr[Array[A]] =
    import quotes.reflect.*

    // Thanks to makkarpov for pointing this out
    Apply(
      TypeApply(
        Select(New(TypeIdent(defn.ArrayClass)), defn.ArrayClass.primaryConstructor),
        List(TypeTree.of[A])
      ),
      List(size.asTerm)
    ).asExprOf[Array[A]]


//  def forallExceptionCart[Tup <: Tuple : Type](tite: Expr[Tuple.Map[Tup, Iterable]], f: Expr[Tup => Boolean])(using Quotes): Expr[Boolean] =
//    val seq = untuple[Iterable[Any]](tite)
//    def unroll[I <: Int : Type](ites: Seq[Expr[Iterable[Any]]], args: Tuple): Expr[Unit] = ites match
//      case Nil => '{ if !${ applyTupleDestruct[Tup, Boolean](args.asInstanceOf, f) } then throw Break }
//      case ite::ites => forEachE(ite.asExprOf[Iterable[Tuple.Elem[Tup, I]]], et => unroll[S[I]](ites, args :* et))
//    '{
//      try
//        ${ unroll[0](seq, EmptyTuple) }
//        true
//      catch
//        case Break => false
//    }
//
//  def zipMap[Tup <: Tuple : Type, R : Type](a: Expr[Array[T]], f: Expr[T => R], n: Expr[Int])(using Quotes): Expr[Array[R]] =
//    val size = n.valueOrAbort
//    '{
//      val na = ${ ofSizeImpl[R](Expr(size)) }
//      var i = 0
//      while i < ${ Expr(size) } do
//        ${ exprTreeTransform[Unit](buildValDefElim)('{ na(i) = ${ betaReduceFixE('{ $f($a(i)) }) } }) }
//        i += 1
//      na
//    }

  def map[T : Type, R : Type](a: Expr[Array[T]], f: Expr[T => R], n: Expr[Int])(using Quotes): Expr[Array[R]] =
    '{
      val na = ${ ofSizeImpl[R](n) }
      var i = 0
      while i < ${ n } do
        ${ exprTreeTransform[Unit](buildValDefElim)('{ na(i) = ${ betaReduceFixE('{ $f($a(i)) }) } }) }
        i += 1
      na
    }

  def mapUnrolled[T : Type, R : Type](a: Expr[Array[T]], f: Expr[T => R], n: Expr[Int])(using Quotes): Expr[Array[R]] =
    val size = n.valueOrAbort
    '{
      val na = ${ ofSizeImpl[R](n) }
      ${
        ArrayIndexImpl.foreachInRange(0, size)(i =>
          '{ na(${ Expr(i) }) = ${ exprTreeTransform[R](buildValDefElim)(betaReduceFixE('{ $f($a(${ Expr(i) })) })) } }
        )
      }
      na
    }

  def mapUnrolledN[T: Type, R: Type](a: Expr[Array[T]], f: Expr[T => R], n: Expr[Int], k: Expr[Int])(using Quotes): Expr[Array[R]] =
    val size = n.valueOrAbort
    val chunk = k.valueOrAbort
    val remaining: Int = size % chunk
    '{
      val na = ${ ofSizeImpl[R](Expr(size)) }
      ${
        ArrayIndexImpl.foreachInRange(0, remaining)(i =>
          exprTreeTransform[Unit](buildValDefElim)('{ na(${ Expr(i) }) = ${ betaReduceFixE('{ $f($a(${ Expr(i) })) }) } })
        )
      }
      var i: Int = ${ Expr(remaining) }
      while i < ${ Expr(size) } do
        ${ ArrayIndexImpl.foreachInRange(0, chunk)(_ => exprTreeTransform[Unit](buildValDefElim)('{ na(i) = ${ betaReduceFixE( '{ $f($a(i)) }) }; i += 1 })) }
      na
    }

  def flatMapFullyUnrolled[T : Type, R : Type](a: Expr[Array[T]], f: Expr[T => Array[R]], n: Expr[Int], m: Expr[Int])(using Quotes): Expr[Array[R]] =
    val size = n.valueOrAbort
    val resultsize = m.valueOrAbort
    '{
    val na = ${ ofSizeImpl[R](Expr(size*resultsize)) }
    ${
    ArrayIndexImpl.foreachInRange(0, size)(i =>
      '{
        val ra = ${ exprTreeTransform[Array[R]](buildValDefElim)(betaReduceFixE('{ $f($a(${ Expr(i) })) })) }
        ${
          ArrayIndexImpl.foreachInRange(0, resultsize)(j =>
            exprTreeTransform[Unit](buildValDefElim)('{ na(${ Expr(i*resultsize + j) }) = ra(${ Expr(j) }) })
          )
        }
      }
    )
    }
    na
    }
