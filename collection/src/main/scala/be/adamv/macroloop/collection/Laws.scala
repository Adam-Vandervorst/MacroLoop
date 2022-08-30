package be.adamv.macroloop.collection

object Laws:
  export scala.compiletime.ops.any.==
  export scala.compiletime.ops.int.*

  erased def assume[X, Y]: X =:= Y = <:<.refl[Any].asInstanceOf[X =:= Y]

  erased given doublecompl[X <: Int, Y <: Int]: (Y =:= (X - (X - Y))) = assume
  erased given zerounit[X <: Int]: (X =:= (X - 0)) = assume
  erased given zerounit_[X <: Int]: ((X - 0) =:= X) = assume
  erased given pred[X, Y](using X =:= Y): Pred[X == Y] = assume
  //given assoc[X <: Int, Y <: Int, Z <: Int]: (X =:= (X - 0)) = <:<.refl[Any].asInstanceOf
  erased given mulpospos[X <: Int, Y <: Int](using Pred[X > 0], Pred[Y > 0]): Pred[X*Y > 0] = assume
  erased given divpospos[X <: Int, Y <: Int](using Pred[X > 0], Pred[Y > 0]): Pred[X/Y > 0] = assume
  erased given ltsubpos[X <: Int, Y <: Int](using Pred[X > Y]): Pred[(X - Y) > 0] = assume
  erased given ltselfsub[X <: Int, Y <: Int](using Pred[Y > 0]): Pred[X > (X - Y)] = assume
  erased given tupleSizePos[Tup <: Tuple]: Pred[Tuple.Size[Tup] > 0] = assume
  type Pred[X <: Boolean] = X <:< true


