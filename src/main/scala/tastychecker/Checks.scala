package tastychecker

import scala.collection.mutable as mut
import tastyquery.Contexts.*
import tastyquery.Trees.*
import tastyquery.Types.*

// -------------------------------------------------------

abstract class Check:
  val name: String = getClass.getSimpleName.init
  def check(tree: Tree)(using Context): List[Problem]

object Check:
  private val _checks: Map[String, Check] =
    List(
      LSP,
      LSPStatements,
      TypeParamBounds
    ).map(x => (x.name, x)).toMap

  def checks(names: List[String]): List[Check] =
    for
      n <- names
      c <- _checks.get(n)
    yield c

  def allChecks: List[Check] = _checks.values.toList

// -------------------------------------------------------

// LSP: Check explicit/fixed/constant types
object LSP extends Check:
  private def checkSubtype(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(tpe) then None else Some(NotSubtype(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case ValDef(_, tpt, rhs, _) =>
        for
          r <- rhs.toList
          p <- checkSubtype(r, tpt.toType)
        yield p
      case DefDef(_, _, resultTpt, rhs, _) =>
        for
          r <- rhs.toList
          p <- checkSubtype(r, resultTpt.toType)
        yield p
      case Apply(fun, args) =>
        for
          (a, t) <- args.zip(fun.tpe.widen.asInstanceOf[MethodType].paramTypes)
          p <- checkSubtype(a, t)
        yield p
      case Typed(expr, tpt) =>
        checkSubtype(expr, tpt.toType).toList
      case Assign(lhs, rhs) =>
        checkSubtype(rhs, lhs.tpe).toList
      case If(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType).toList
      case InlineIf(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType).toList
      case Lambda(meth, tpt) =>
        for
          t <- tpt.toList
          p <- checkSubtype(meth, t.toType)
        yield p
      case Match(selector, cases) =>
        checkSubtype(selector, defn.MatchableType).toList
      case InlineMatch(selector, cases) =>
        for
          s <- selector.toList
          p <- checkSubtype(s, defn.MatchableType)
        yield p
      case CaseDef(_, guard, _) =>
        for
          g <- guard.toList
          p <- checkSubtype(g, defn.BooleanType)
        yield p
      case SeqLiteral(elems, elempt) =>
        for
          e <- elems
          p <- checkSubtype(e, elempt.toType)
        yield p
      case While(cond, _) =>
        checkSubtype(cond, defn.BooleanType).toList
      case Throw(expr) =>
        checkSubtype(expr, defn.ThrowableType).toList
      case Return(expr, from) =>
        for
          e <- expr.toList
          p <- checkSubtype(e, from.declaredType)
        yield p
      case _ => Nil

// -------------------------------------------------------

// LSP: Check statement types (Any)
object LSPStatements extends Check:
  private def checkAny(term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(defn.AnyType) then None else Some(NotSubtype(term.tpe, defn.AnyType, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case Template(_, _, _, body) =>
        for
          case b: TermTree <- body
          p <- checkAny(b)
        yield p
      case Block(stats, _) =>
        for
          case s: TermTree <- stats
          p <- checkAny(s)
        yield p
      case Try(_, _, finalizer) =>
        for
          f <- finalizer.toList
          p <- checkAny(f)
        yield p
      case While(_, body) =>
        checkAny(body).toList
      case _ => Nil

// -------------------------------------------------------

object TypeParamBounds extends Check:
  private def checkBounds(tpe: TypeTree, bounds: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsToBounds] =
    if bounds.contains(tpe.toType) then None else Some(NotConformsToBounds(tpe.toType, bounds, tree))

  override def check(tree: Tree)(using Context): List[NotConformsToBounds] =
    given Tree = tree
    tree match
      case TypeApply(fun, args) =>
        for
          (a, t) <- args.zip(fun.tpe.widen.asInstanceOf[PolyType].paramTypeBounds)
          p <- checkBounds(a, t)
        yield p
      case _ => Nil
