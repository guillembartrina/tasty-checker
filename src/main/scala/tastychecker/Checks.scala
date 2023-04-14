package tastychecker

import scala.annotation.tailrec

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Flags.*
import tastyquery.Symbols.*
import tastyquery.Exceptions.*
import tastyquery.Trees.*
import tastyquery.Types.*

abstract class Check:
  val name: String = getClass.getSimpleName.init
  def check(tree: Tree)(using Context): List[Problem]

object Check:
  private val _checks: Map[String, Check] =
    List(
      LSP,
      LSPStatements,
      PseudoLSP,
      PseudoLSPMatching,
      TypeParamBounds
    ).map(x => (x.name, x)).toMap

  def checks(names: List[String]): List[Check] =
    for
      n <- names
      c <- _checks.get(n)
    yield c

  def allChecks: List[Check] = _checks.values.toList


// -------------------------------------------------------
// LSP

trait MethodsLSP:
  protected def checkSubtype(tpea: Type, tpeb: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    if tpea.isSubtype(tpeb) then None else Some(NotSubtype(tpea, tpeb, tree))

  protected def checkSubtype(terma: TermTree, termb: TermTree)(using Tree)(using Context): Option[NotSubtype] =
    checkSubtype(terma.tpe, termb.tpe)

  protected def checkSubtype(tpe: Type, term: TermTree)(using Tree)(using Context): Option[NotSubtype] =
    checkSubtype(tpe, term.tpe)

  protected def checkSubtype(term: TermTree, tpe: Type)(using Tree)(using Context): Option[NotSubtype] =
    checkSubtype(term.tpe, tpe)


// LSP: Check explicit/fixed/constant types
object LSP extends Check with MethodsLSP:
  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case CaseDef(_, guard, _) =>
        for
          g <- guard
          p <- checkSubtype(g, defn.BooleanType)
        yield p
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
      case Apply(fun, args) =>
        def cleanParamType(ptpe: Type): Type = ptpe match
          case ptpe: ByNameType => ptpe.underlying
          case _ => ptpe
        val paramTypes = fun.tpe.widen.asInstanceOf[MethodType].paramTypes.map(cleanParamType)
        for
          (a, ptpe) <- args.zip(paramTypes)
          p <- checkSubtype(a, ptpe)
        yield p
      case Assign(lhs, rhs) =>
        checkSubtype(rhs, lhs.tpe.widen)
      case If(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType)
      case InlineIf(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType)
      case Lambda(meth, tpt) =>
        // TODO: Finish, remove temporal fix
        if tpt.isDefined && tpt.get.toType.widen.isInstanceOf[AppliedType] then return Nil
        def getSAMType(tpe: Type): Type =
          val sams = tpe.asInstanceOf[TypeRef].optSymbol.get.asClass.declarations.filter(x => x.flags.is(Abstract)).map(_.asTerm)
          if sams.size == 1
          then sams.head.declaredType
          else sams.find(_.name == termName("apply")).get.declaredType
        for
          tpt <- tpt
          p <- checkSubtype(meth, getSAMType(tpt.toType.widen))
        yield p
      case Return(expr, from) =>
        for
          e <- expr.toList
          p <- checkSubtype(e, from.declaredType.asInstanceOf[MethodType].resultType)
        yield p
      case SeqLiteral(elems, elempt) =>
        for
          el <- elems
          p <- checkSubtype(el, elempt.toType)
        yield p
      case Super(qual, mix) =>
        for
          tpt <- mix
          p <- checkSubtype(qual, tpt.toType)
        yield p
      case Throw(expr) =>
        checkSubtype(expr, defn.ThrowableType)
      case Typed(expr, tpt) =>
        if tpt.toType.isOfClass(defn.RepeatedParamClass)
        then checkSubtype(expr, defn.SeqTypeOf(tpt.toType.asInstanceOf[AppliedType].args(0)))
        else checkSubtype(expr, tpt.toType)
      case While(cond, _) =>
        checkSubtype(cond, defn.BooleanType)
      case _ => Nil
    ret.iterator.to(List)

// LSP: Check statement types (Any)
object LSPStatements extends Check with MethodsLSP:
  protected def checkAny(term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(term, defn.AnyType)

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
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
      case Match(selector, _) =>
        checkAny(selector)
      case InlineMatch(selector, _) =>
        for
          s <- selector
          p <- checkAny(s)
        yield p
      case Try(expr, _, finalizer) =>
        checkAny(expr) ++ {
          for
            f <- finalizer
            p <- checkAny(f)
          yield p
        }
      case While(_, body) =>
        checkAny(body)
      case _ => Nil
    ret.iterator.to(List)

// PseudoLSP: Other LSP invariants (top-down)
object PseudoLSP extends Check with MethodsLSP:
  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case tr @ Apply(fun, _) =>
        @tailrec
        def isApplyNew(tree: TermTree)(using Context): Boolean = tree match
          case Apply(fn, _) => isApplyNew(fn)
          case TypeApply(fn, _) => isApplyNew(fn)
          case Block(_, expr) => isApplyNew(expr)
          case Select(New(_), SignedName(nme.Constructor, _, _)) => true
          case _ => false
        if !isApplyNew(tr)
        then checkSubtype(fun.tpe.widen.asInstanceOf[MethodType].resultType, tr)
        else None
      case tr @ Block(_, expr) =>
        checkSubtype(expr, tr)
      case tr @ If(_, thenPart, elsePart) =>
        checkSubtype(thenPart, tr) ++ checkSubtype(elsePart, tr)
      case tr @ InlineIf(_, thenPart, elsePart) =>
        checkSubtype(thenPart, tr) ++ checkSubtype(elsePart, tr)
      case tr @ InlineMatch(_, cases) =>
        for
          c <- cases
          p <- checkSubtype(c.body, tr)
        yield p
      case tr @ Inlined(expr, _, _) =>
        checkSubtype(expr, tr)
      case tr @ Match(_, cases) =>
        for
          c <- cases
          p <- checkSubtype(c.body, tr)
        yield p
      case tr @ NamedArg(_, arg) =>
        checkSubtype(arg, tr)
      case tr @ Try(expr, cases, _) =>
        checkSubtype(expr, tr) ++
        {
          for
            c <- cases
            p <- checkSubtype(c.body, tr)
          yield p
        }
      case _ => Nil
    ret.iterator.to(List)

// PseudoLSPMatching: LSP invariants within matchings
object PseudoLSPMatching extends Check with MethodsLSP:
  private def checkPattern(pattern: PatternTree)(using Tree)(using Context): List[NotSubtype] =
    def rec(pat: PatternTree): (Type, List[NotSubtype]) =
      given Tree = pat
      pat match
        case Bind(_, body, symbol) =>
          val (tpe, problems) = rec(body)
          (tpe, problems ++ checkSubtype(tpe, symbol.declaredType.widen)(using pat))
        case Alternative(trees) =>
          val (tpes, problems) = trees.map(rec(_)).unzip
          (tpes.reduce(OrType(_, _)), problems.flatten)
        case ExprPattern(expr) =>
          (expr.tpe, Nil)
        case WildcardPattern(tpe) =>
          (tpe, Nil)
        case TypeTest(body, tpt) =>
          val (tpe, problems) = rec(body)
          (tpt.toType, problems ++ checkSubtype(tpe, tpt.toType)(using pat))
        case Unapply(fun, _, patterns) =>
          val problems = patterns.map(checkPattern(_))
          (fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0), problems.flatten)
    rec(pattern)._2

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case Match(_, cases) =>
        for
          c <- cases.map(_.pattern)
          p <- checkPattern(c)
        yield p
      case InlineMatch(_, cases) =>
        for
          c <- cases.map(_.pattern)
          p <- checkPattern(c)
        yield p
      case Try(_, cases, _) =>
        for
          c <- cases.map(_.pattern)
          p <- checkPattern(c)
        yield p
      case _ => Nil
    ret.iterator.to(List)

// -------------------------------------------------------
// TypeParamBounds

object TypeParamBounds extends Check:
  private def checkBounds(tpe: TypeTree, bounds: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsToBounds] =
    if bounds.contains(tpe.toType) then None else Some(NotConformsToBounds(tpe.toType, bounds, tree))

  override def check(tree: Tree)(using Context): List[NotConformsToBounds] =
    given Tree = tree
    tree match
      case TypeApply(fun, args) =>
        for
          (a, b) <- args.zip(fun.tpe.widen.asInstanceOf[PolyType].paramTypeBounds)
          p <- checkBounds(a, b)
        yield p
      case _ => Nil
