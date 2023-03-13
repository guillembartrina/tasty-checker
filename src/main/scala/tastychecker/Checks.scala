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
      LSPComp,
      LSPStat
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
      case CaseDef(_, guard, _) =>
        for
          g <- guard.toList
          p <- checkSubtype(g, defn.BooleanType)
        yield p
      case DefDef(_, _, resultTpt, rhs, _) =>
        for
          r <- rhs.toList
          p <- checkSubtype(r, resultTpt.toType)
        yield p
      case ValDef(_, tpt, rhs, _) =>
        for
          r <- rhs.toList
          p <- checkSubtype(r, tpt.toType)
        yield p
      case Apply(fun, args) =>
        for
          (a, t) <- args.zip(fun.tpe.widen.asInstanceOf[MethodType].paramTypes)
          p <- checkSubtype(a, t)
        yield p
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
      case Return(expr, from) =>
        for
          e <- expr.toList
          p <- checkSubtype(e, from.declaredType)
        yield p
      case SeqLiteral(elems, elempt) =>
        for
          e <- elems
          p <- checkSubtype(e, elempt.toType)
        yield p
      case Throw(expr) =>
        checkSubtype(expr, defn.ThrowableType).toList
      case Try(_, _, finalizer) =>
        for
          f <- finalizer.toList
          p <- checkSubtype(f, defn.UnitType)
        yield p
      case Typed(expr, tpt) =>
        checkSubtype(expr, tpt.toType).toList
      case While(cond, _) =>
        checkSubtype(cond, defn.BooleanType).toList
      case _ => Nil

// -------------------------------------------------------

// LSP: Check computed types
object LSPComp extends Check:
  private def checkSubtype(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(tpe) then None else Some(NotSubtype(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case t @ Apply(fun, args) =>
        //val ttpe = fun.tpe.asInstanceOf[MethodType].resultType
        //if ttpe.isSubtype(t.tpe) then Nil else NotSubtype(ttpe, t.tpe, tree) :: Nil
        Nil
      case t @ Block(_, expr) =>
        checkSubtype(expr, t.tpe).toList
      case t @ If(_, thenPart, elsePart) =>
        checkSubtype(thenPart, t.tpe).toList ::: checkSubtype(elsePart, t.tpe).toList
      case t @ InlineIf(_, thenPart, elsePart) =>
        checkSubtype(thenPart, t.tpe).toList ::: checkSubtype(elsePart, t.tpe).toList
      case t @ InlineMatch(_, cases) =>
        for
          b <- cases.map(_.body)
          p <- checkSubtype(b, t.tpe)
        yield p
      case t @ Inlined(expr, _, _) =>
        checkSubtype(expr, t.tpe).toList
      case t @ Match(_, cases) =>
        for
          b <- cases.map(_.body)
          p <- checkSubtype(b, t.tpe)
        yield p
      case t @ TypeApply(fun, args) =>
        checkSubtype(fun, t.tpe).toList
      case t @ Try(expr, cases, finalizer) =>
        checkSubtype(expr, t.tpe).toList ::: {
          for
            b <- cases.map(_.body)
            p <- checkSubtype(b, t.tpe)
          yield p
        }
      case _ => Nil

// -------------------------------------------------------

// LSP: Check statement types (Any)
object LSPStat extends Check:
  private def checkAny(term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(defn.AnyType) then None else Some(NotSubtype(term.tpe, defn.AnyType, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case Template(_, _, _, body) =>
        for
          case b: TermTree <- body
          p <- checkAny(b).toList
        yield p
      case Block(stats, _) =>
        for
          case s: TermTree <- stats
          p <- checkAny(s).toList
        yield p
      case Try(_, _, finalizer) =>
        for
          f <- finalizer.toList
          p <- checkAny(f).toList
        yield p
      case While(_, body) =>
        checkAny(body).toList
      case _ => Nil
