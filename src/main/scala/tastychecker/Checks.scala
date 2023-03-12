package tastychecker

import tastyquery.Contexts.*
import tastyquery.Trees.*
import tastyquery.Types.*

// -------------------------------------------------------

abstract class Check(val name: String):
  def check(tree: Tree)(using Context): List[Problem]

// -------------------------------------------------------

// LSP: Check explicit/fixed/constant types
object LSP extends Check("LSP"):
  private def runCheck(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(tpe) then None else Some(NotSubtype(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case CaseDef(_, guard, _) =>
        for
          g <- guard.toList
          p <- runCheck(g, defn.BooleanType)
        yield p
      case DefDef(_, _, resultTpt, rhs, _) =>
        for
          r <- rhs.toList
          p <- runCheck(r, resultTpt.toType)
        yield p
      case ValDef(_, tpt, rhs, _) =>
        for
          r <- rhs.toList
          p <- runCheck(r, tpt.toType)
        yield p
      case Apply(fun, args) =>
        for
          (a, t) <- args.zip(fun.tpe.widen.asInstanceOf[MethodType].paramTypes)
          p <- runCheck(a, t)
        yield p
      case Assign(lhs, rhs) =>
        runCheck(rhs, lhs.tpe).toList
      case If(cond, _, _) =>
        runCheck(cond, defn.BooleanType).toList
      case InlineIf(cond, _, _) =>
        runCheck(cond, defn.BooleanType).toList
      case Lambda(meth, tpt) =>
        for
          t <- tpt.toList
          p <- runCheck(meth, t.toType)
        yield p
      case Return(expr, from) =>
        for
          e <- expr.toList
          p <- runCheck(e, from.declaredType)
        yield p
      case SeqLiteral(elems, elempt) =>
        for
          e <- elems
          p <- runCheck(e, elempt.toType)
        yield p
      case Throw(expr) =>
        runCheck(expr, defn.ThrowableType).toList
      case Try(_, _, finalizer) =>
        for
          f <- finalizer.toList
          p <- runCheck(f, defn.UnitType)
        yield p
      case Typed(expr, tpt) =>
        runCheck(expr, tpt.toType).toList
      case While(cond, _) =>
        runCheck(cond, defn.BooleanType).toList
      case _ => Nil

// -------------------------------------------------------

// LSP: Check computed types
object LSPComp extends Check("LSPComp"):
  private def runCheck(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(tpe) then None else Some(NotSubtype(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case t @ Apply(fun, args) =>
        //val ttpe = fun.tpe.asInstanceOf[MethodType].resultType
        //if ttpe.isSubtype(t.tpe) then Nil else NotSubtype(ttpe, t.tpe, tree) :: Nil
        Nil
      case t @ Block(_, expr) =>
        runCheck(expr, t.tpe).toList
      case t @ If(_, thenPart, elsePart) =>
        runCheck(thenPart, t.tpe).toList ::: runCheck(elsePart, t.tpe).toList
      case t @ InlineIf(_, thenPart, elsePart) =>
        runCheck(thenPart, t.tpe).toList ::: runCheck(elsePart, t.tpe).toList
      case t @ InlineMatch(_, cases) =>
        for
          b <- cases.map(_.body)
          p <- runCheck(b, t.tpe)
        yield p
      case t @ Inlined(expr, _, _) =>
        runCheck(expr, t.tpe).toList
      case t @ Match(_, cases) =>
        for
          b <- cases.map(_.body)
          p <- runCheck(b, t.tpe)
        yield p
      case t @ TypeApply(fun, args) =>
        runCheck(fun, t.tpe).toList
      case t @ Try(expr, cases, finalizer) =>
        runCheck(expr, t.tpe).toList ::: {
          for
            b <- cases.map(_.body)
            p <- runCheck(b, t.tpe)
          yield p
        }
      case _ => Nil

// -------------------------------------------------------

// LSP: Check statement types (Any)
object LSPStat extends Check("LSPStat"):
  private def runCheck(term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    if term.tpe.isSubtype(defn.AnyType) then None else Some(NotSubtype(term.tpe, defn.AnyType, tree))

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    tree match
      case Template(_, _, _, body) =>
        for
          case b: TermTree <- body
          p <- runCheck(b).toList
        yield p
      case Block(stats, _) =>
        for
          case s: TermTree <- stats
          p <- runCheck(s).toList
        yield p
      case Try(_, _, finalizer) =>
        for
          f <- finalizer.toList
          p <- runCheck(f).toList
        yield p
      case While(_, body) =>
        runCheck(body).toList
      case _ => Nil
