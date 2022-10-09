package be.adamv.macroloop.deriving

trait Reconstructable[F[_]]:
  extension [A](fa: F[A])
    def reconstructFromTuple[B](tup: Tuple): F[B]

object Reconstructable:
  inline given derived[F[_]]: Reconstructable[F] = ${ macros.deriveReconstructable[F] }
