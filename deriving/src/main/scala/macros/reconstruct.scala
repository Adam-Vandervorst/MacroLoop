package be.adamv.macroloop.deriving.macros

import scala.deriving.*
import scala.quoted.*

import be.adamv.macroloop.deriving.Reconstructable
import be.adamv.macroloop.ConstantTuple.constToList

private object Hole

def buildIndex[T: Type](c: Int = 0)(using Quotes): List[Int] =
  Type.of[T] match
    case '[Hole.type *: tpes] => c :: buildIndex[tpes](c + 1)
    case '[_ *: tpes]    => -1 :: buildIndex[tpes](c)
    case '[EmptyTuple]     => Nil

def deriveReconstructable[F[_]: Type](using q: Quotes): Expr[Reconstructable[F]] =
  import q.reflect.*

  val ev: Expr[Mirror.Of[F[Hole.type]]] = Expr.summon[Mirror.Of[F[Hole.type]]].get
    ev match
      case '{ $m: Mirror.ProductOf[F[Hole.type]] { type MirroredElemTypes = elementTypes }} =>
        val index = buildIndex[elementTypes]()
        val cls = TypeRepr.of[F].classSymbol.get
        val newCls = Select(New(TypeIdent(cls)), cls.primaryConstructor)
        val fields = cls.caseFields

        def body[A:Type, B:Type](fa: Expr[F[A]], tup: Expr[Tuple]): Expr[F[B]] =
          val args: List[Term] = index.zipWithIndex.map {
            case (-1, j) => fa.asTerm.select(fields(j))
            case (i, _) => '{ $tup.productElement(${ Expr(i) }).asInstanceOf[B] }.asTerm
          }
          newCls.appliedToType(TypeRepr.of[B]).appliedToArgs(args).asExprOf[F[B]]

        '{
          new Reconstructable[F]:
            extension [A](fa: F[A])
              def reconstructFromTuple[B](tup: Tuple): F[B] =
                ${ body('fa, 'tup) }
        }
