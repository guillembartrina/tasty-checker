package tastychecker

import tastyquery.Contexts.*
import tastyquery.Names.*
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
      LSPCalculated,
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

trait MethodsLSP:
  protected def checkSubtype(tpea: Type, tpeb: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    if tpea.isSubtype(tpeb) then None else Some(NotSubtype(tpea, tpeb, tree))

  protected def checkSubtype(terma: TermTree, termb: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(terma.tpe, termb.tpe)

  protected def checkSubtype(tpe: Type, term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(tpe, term.tpe)

  protected def checkSubtype(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(term.tpe, tpe)

// LSP: Check explicit/fixed/constant types
object LSP extends Check with MethodsLSP:
  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case ValDef(_, tpt, rhs, _) =>
        for
          r <- rhs
          p <- checkSubtype(r, tpt.toType)
        yield p
      case DefDef(_, _, resultTpt, rhs, _) =>
        for
          r <- rhs
          p <- checkSubtype(r, resultTpt.toType)
        yield p
      case Super(qual, mix) =>
        for
          t <- mix
          p <- checkSubtype(qual, t.toType)
        yield p
      case Apply(fun, args) =>
        for
          (a, t) <- args.zip(fun.tpe.widen.asInstanceOf[MethodType].paramTypes)
          p <- checkSubtype(a, t)
        yield p
      case Typed(expr, tpt) =>
        checkSubtype(expr, tpt.toType)
      case tr @ Assign(lhs, rhs) =>
        checkSubtype(rhs, lhs.tpe.widen) ++ checkSubtype(tr, defn.UnitType)
      case If(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType)
      case InlineIf(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType)
      case Lambda(meth, tpt) =>
        for
          t <- tpt
          p <- checkSubtype(meth, t.toType)
        yield p
      case CaseDef(_, guard, _) =>
        for
          g <- guard
          p <- checkSubtype(g, defn.BooleanType)
        yield p
      case tr @ SeqLiteral(elems, elempt) =>
        {
          for
            e <- elems
            p <- checkSubtype(e, elempt.toType)
          yield p
        } ++ checkSubtype(tr, defn.SeqTypeOf(defn.AnyType))
      case tr @ While(cond, _) =>
        checkSubtype(cond, defn.BooleanType) ++ checkSubtype(tr, defn.UnitType)
      case tr @ Throw(expr) =>
        checkSubtype(expr, defn.ThrowableType) ++ checkSubtype(tr, defn.NothingType)
      case tr @ Return(expr, from) =>
        {
          for
            e <- expr.toList
            p <- checkSubtype(e, from.declaredType.asInstanceOf[MethodType].resultType)
          yield p
        } ++ checkSubtype(tr, defn.NothingType).toList
      case _ => Nil
    ret.toList

// -------------------------------------------------------

// LSP: Check computed types
object LSPCalculated extends Check with MethodsLSP:
  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case tr @ Apply(fun, _) =>
        fun match
          case Select(newObj @ New(_), SignedName(nme.Constructor, _, _)) =>
            None
          case _ =>
            checkSubtype(fun.tpe.widen.asInstanceOf[MethodType].resultType, tr.tpe)
      case tr @ Typed(expr, tpt) =>
        checkSubtype(expr, tr) ++ checkSubtype(tpt.toType, tr)
      case tr @ NamedArg(_, arg) =>
        checkSubtype(arg, tr)
      case tr @ Block(_, expr) =>
        checkSubtype(expr, tr)
      case tr @ If(_, thenPart, elsePart) =>
        checkSubtype(thenPart, tr) ++ checkSubtype(elsePart, tr)
      case tr @ InlineIf(_, thenPart, elsePart) =>
        checkSubtype(thenPart, tr) ++ checkSubtype(elsePart, tr)
      case tr @ Lambda(meth, tpt) =>
        checkSubtype(meth, tr) ++ {
          for
            t <- tpt
            p <- checkSubtype(t.toType, tr)
          yield p
        }
      case tr @ Match(_, cases) =>
        for
          b <- cases.map(_.body)
          p <- checkSubtype(b, tr)
        yield p
      case tr @ InlineMatch(_, cases) =>
        for
          b <- cases.map(_.body)
          p <- checkSubtype(b, tr)
        yield p
      case tr @ SeqLiteral(elems, elemtpt) =>
        val tpe = tr.tpe.asInstanceOf[AppliedType].args(0)
        checkSubtype(elemtpt.toType, tpe) ++ {
          for
            e <- elems
            p <- checkSubtype(e, tpe)
          yield p
        }
      case tr @ Try(expr, cases, finalizer) =>
        checkSubtype(expr, tr) ++ {
          for
            b <- cases.map(_.body)
            p <- checkSubtype(b, tr)
          yield p
        }
      case tr @ Inlined(expr, _, _) =>
        checkSubtype(expr, tr)
      case tr @ Literal(constant) =>
        checkSubtype(constant.wideType, tr.tpe.widen)
      case _ => Nil
    ret.toList

// -------------------------------------------------------

// LSP: Check statement types (Any)
object LSPStatements extends Check with MethodsLSP:
  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case Template(_, _, _, body) =>
        for
          case b: TermTree <- body
          p <- checkSubtype(b, defn.AnyType)
        yield p
      case Block(stats, _) =>
        for
          case s: TermTree <- stats
          p <- checkSubtype(s, defn.AnyType)
        yield p
      case Match(selector, _) =>
        checkSubtype(selector, defn.AnyType)
      case InlineMatch(selector, _) =>
        for
          s <- selector
          p <- checkSubtype(s, defn.AnyType)
        yield p
      case Try(expr, _, finalizer) =>
        checkSubtype(expr, defn.AnyType) ++ {
          for
            f <- finalizer
            p <- checkSubtype(f, defn.AnyType)
          yield p
        }
      case While(_, body) =>
        checkSubtype(body, defn.AnyType)
      case _ => Nil
    ret.toList

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
