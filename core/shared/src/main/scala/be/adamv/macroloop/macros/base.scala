package be.adamv.macroloop.macros

import be.adamv.macroloop.utils.*
import scala.quoted.*


transparent inline def exprTransform[Y : Type](using q: Quotes)(f: q.reflect.Term => q.reflect.Term)(e: Expr[_]): Expr[Y] =
  import q.reflect.asTerm
  f(e.asTerm).asExprOf[Y]

transparent inline def exprTreeTransform[Y : Type](using q: Quotes)(f: q.reflect.TreeMap)(e: Expr[_]): Expr[Y] =
  import q.reflect.*
  f.transformTerm(e.asTerm)(Symbol.spliceOwner).asExprOf[Y]

def buildRefRemap(using q: Quotes)(mapping: Map[q.reflect.Symbol, q.reflect.Term]): q.reflect.TreeMap =
  import q.reflect.*
  new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case i: Ident => mapping.unapply(i.symbol).getOrElse(i)
      case _ => super.transformTerm(tree)(owner)

def buildValDefElim(using q: Quotes): q.reflect.TreeMap =
  import q.reflect.{ValDefTypeTest, *}
  new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case Block(vds, e) if vds.forall(vd => ValDefTypeTest.unapply(vd).nonEmpty) =>
        val mapping = vds.collect{
          case vd @ ValDef(n, _, Some(t))
            if !buildTrivialInlineElim().transformTerm(t)(owner).symbol.flags.is(Flags.Method)
              || gatherRefs(e).count(_ == vd.symbol) <= 1 =>
            vd.symbol -> t
        }.toMap
        if mapping.isEmpty then super.transformTerm(tree)(owner)
        else buildRefRemap(mapping).transformTerm(e)(owner)
      case _ => super.transformTerm(tree)(owner)

def buildTrivialInlineElim(using q: Quotes)(simplifyNamed: Boolean = false): q.reflect.TreeMap =
  import q.reflect.*
  new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case Inlined(None, Nil, b) => transformTerm(b)(owner)
      case Inlined(Some(Ident(_)), Nil, b) if simplifyNamed => transformTerm(b)(owner)
      case _ => super.transformTerm(tree)(owner)

def constantFoldSelected(using q: Quotes)(x: q.reflect.Term, reduction: (q.reflect.Symbol, String) => Option[q.reflect.Term]): q.reflect.Term =
  import q.reflect.*
  val rewr = new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case s @ Select(q, name) => reduction(q.symbol, name).fold(Select.copy(tree)(transformTerm(q)(owner), name))(transformTerm(_)(owner))
      case _ => super.transformTerm(tree)(owner)
  rewr.transformTerm(x)(Symbol.spliceOwner)

def gatherValDefSymbols(using q: Quotes)(x: q.reflect.Term): List[q.reflect.Symbol] =
  import collection.mutable.ListBuffer
  import q.reflect.*
  val acc = new TreeAccumulator[ListBuffer[Symbol]]:
    def foldTree(syms: ListBuffer[Symbol], tree: Tree)(owner: Symbol): ListBuffer[Symbol] = tree match
      case vdef @ ValDef(_, _, rhs) => rhs.map(t => foldTree(syms, t)(owner)).getOrElse(syms).append(vdef.symbol)
      case _ => foldOverTree(syms, tree)(owner)
  acc.foldTree(ListBuffer.empty, x)(Symbol.spliceOwner).result()

def gatherRefs(using q: Quotes)(x: q.reflect.Term): List[q.reflect.Symbol] =
  import collection.mutable.ListBuffer
  import quotes.reflect.*
  val acc = new q.reflect.TreeAccumulator[ListBuffer[Symbol]]:
    def foldTree(syms: ListBuffer[Symbol], tree: Tree)(owner: Symbol): ListBuffer[Symbol] = tree match
      case ref: Ref => ref.symbol +: syms
      case _ => foldOverTree(syms, tree)(owner)
  acc.foldTree(ListBuffer.empty, x)(Symbol.spliceOwner).result()

def translateRefs(using q: Quotes)(mapping: Map[q.reflect.Symbol, q.reflect.Symbol])(x: q.reflect.Term): q.reflect.Term =
  // TODO this can fail and should return an option
  import quotes.reflect.*
  val rewr = new TreeMap:
    override def transformStatement(tree: Statement)(owner: Symbol): Statement = tree match
      case d @ ValDef(_, _, rhs) => ValDef(mapping(d.symbol), rhs.map(transformTerm(_)(owner))): Statement
      case _ => super.transformStatement(tree)(owner)
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case i: Ident => mapping.unapply(i.symbol).fold(i)(s => Ident(s.termRef))
      case _ => super.transformTerm(tree)(owner)
  rewr.transformTerm(x)(Symbol.spliceOwner)

def renameBoundFrom(using q: Quotes)(toRename: q.reflect.Term, niceNames: q.reflect.Term): q.reflect.Term =
  val sourceNames = gatherValDefSymbols(toRename)
  val targetNames = gatherValDefSymbols(niceNames)
  // TODO this can fail and should flatMap into an option
  assert(sourceNames.length == targetNames.length, "different number of ValDefs")
  assert(sourceNames.occurrences == targetNames.occurrences, "ValDef occurrences different")
  val mapping = (sourceNames zip targetNames).toMap
  translateRefs(mapping)(toRename)

def unlist(xs: Expr[List[Any]])(using Quotes): List[Expr[Any]] = xs match
  case '{ $x :: ($xs: List[Any]) } => x :: unlist(xs)
  case '{ Nil } => Nil

def untuple[B : Type](e: Expr[Tuple])(using Quotes): Seq[Expr[B]] =
  import quotes.reflect.*
  def rec(tree: Term): Seq[Expr[B]] = tree match
    case Repeated(elems, _) => elems.map(_.asExprOf[B])
    case Typed(e, _) => rec(e)
    case Inlined(_, Nil, e) => rec(e)
    // UNSAFE, assume tuple
    case Apply(_, args) => args.map(_.asExprOf[B])
    case _ => report.errorAndAbort(s"couldn't untuple tree ${tree.show}")
  rec(stripCast(e.asTerm))

def tupleToExpr[Tup <: Tuple : Type](t: Tuple.Map[Tup, Expr])(using Quotes): Expr[Tup] =
  import quotes.reflect.*
  val terms = t.productIterator.map(_.asInstanceOf[Expr[Any]].asTerm).toList
  val types = terms.map(_.tpe.widen)
  val tupleObject = Seq('{Tuple1}, '{Tuple2}, '{Tuple3}, '{Tuple4}, '{Tuple5}, '{Tuple6}, '{Tuple7}, '{Tuple8}, '{Tuple9}, '{Tuple10})(terms.length - 1)
  val tupleApply = Select.unique(tupleObject.asTerm, "apply")
  Apply(TypeApply(tupleApply, types.map(Inferred(_))), terms).asExprOf[Tup]

def lambdaDestruct(using q: Quotes)(f: q.reflect.Term)(owner: q.reflect.Symbol): Option[(q.reflect.Symbol, q.reflect.Term)] =
  import quotes.reflect.*
  buildTrivialInlineElim().transformTerm(f)(Symbol.spliceOwner) match
    case Block(List(DefDef(_, List(TermParamClause(List(vdef))), _, Some(body))), _: q.reflect.Closure) => Some((vdef.symbol, body))
    case _ => None

def matchBindings(using q: Quotes)(m: q.reflect.Term)(owner: q.reflect.Symbol): Option[(List[q.reflect.Symbol], q.reflect.Term)] =
  import quotes.reflect.*
  m match
    case Match(Ident(_), List(q.reflect.CaseDef(Unapply(fun, List(), bindings), None, rhs))) =>
      Some((bindings.map(_.symbol), rhs))
    case _ => None

def tupleFieldName(s: String): Int =
  assert(s.startsWith("_"))
  s.tail.toInt

def applyTupleDestruct[Tup <: Tuple : Type, R : Type](t: Tuple.Map[Tup, Expr], f: Expr[Tup => R])(using q: Quotes): Expr[R] =
  import quotes.reflect.*
  import reflect.Selectable.reflectiveSelectable
  lambdaDestruct(f.asTerm)(Symbol.spliceOwner) match
    case Some((symbol, body)) =>
      matchBindings(body)(Symbol.spliceOwner) match // TODO tighten matching
        case Some((bindings, rhs)) =>
          assert(bindings.length == t.size)
          buildRefRemap(bindings.zip(t.toList.map(_.asInstanceOf[Expr[Any]].asTerm)).toMap).transformTerm(rhs)(Symbol.spliceOwner).asExprOf[R]
        case None =>
          // TODO this can still be an automatic expansion, which will crash the solution below
          val reduced = constantFoldSelected(body, (x, n) =>
            Option.when(symbol == x)(t.asInstanceOf[NonEmptyTuple].apply(tupleFieldName(n)).asInstanceOf[Expr[Any]].asTerm))
          // TODO constant fold apply too
          reduced.asExprOf[R]
    case None =>
      '{ $f(${ tupleToExpr[Tup](t) }) }

extension [X : Type](initial: Expr[X])
  def mutating[Y : Type](cont: (Expr[X], Expr[X => Unit]) => Expr[Y])(using Quotes): Expr[Y] = '{
    var i = $initial
    ${ cont('i, '{ i = _ }) }
  }

import quoted.FromExpr.BooleanFromExpr
val PrimitiveFromExpr = quoted.FromExpr.BooleanFromExpr.asInstanceOf[FromExpr[Const]]

def betaReduceFixE[T](e: Expr[T])(using Quotes): Expr[T] = fix((x: Expr[T]) => Expr.betaReduce(x))(e)


object Break extends Exception
