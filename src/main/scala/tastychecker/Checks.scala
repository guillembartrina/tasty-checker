package tastychecker

import scala.annotation.tailrec

import tastyquery.Contexts.*
import tastyquery.Names.*
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

  protected def checkSubtype(terma: TermTree, termb: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(terma.tpe, termb.tpe)

  protected def checkSubtype(tpe: Type, term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(tpe, term.tpe)

  protected def checkSubtype(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(term.tpe, tpe)

  protected def literalTypes(using Context): List[Type] = List(
    defn.UnitType, defn.BooleanType, defn.CharType, defn.ByteType, defn.ShortType,
    defn.IntType, defn.LongType, defn.FloatType, defn.DoubleType, defn.StringType,
    defn.NullType, defn.ClassTypeOf(defn.AnyType)
  )

  protected def asMethodType(t: TermTree | TermSymbol)(using Context): MethodType = t match
    case t: TermTree => t.tpe.widen.asInstanceOf[MethodType]
    case t: TermSymbol => t.declaredType.asInstanceOf[MethodType]

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
        def prepParamTypes(p: Type): Type = p match
          case p: ByNameType => p.underlying
          case _ => p
        val cleanParamTypes = asMethodType(fun).paramTypes.map(prepParamTypes)
        for
          (a, t) <- args.zip(cleanParamTypes)
          p <- checkSubtype(a, t)
        yield p
      case Typed(expr, tpt) =>
        // checkSubtype(expr, tpt.toType)
        // Temporal fix
        if tpt.toType.isOfClass(defn.RepeatedParamClass)
        then checkSubtype(expr, defn.SeqTypeOf(tpt.toType.asInstanceOf[AppliedType].args(0)))
        else checkSubtype(expr, tpt.toType)
      case tr @ Assign(lhs, rhs) =>
        checkSubtype(rhs, lhs.tpe.widen) ++ checkSubtype(tr, defn.UnitType)
      case If(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType)
      case InlineIf(cond, _, _) =>
        checkSubtype(cond, defn.BooleanType)
      case Lambda(meth, tpt) => // REDO
        /*
        for
          t <- tpt
          p <- checkSubtype(meth, t.toType)
        yield p
        */
        Nil
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
            p <- checkSubtype(e, asMethodType(from).resultType)
          yield p
        } ++ checkSubtype(tr, defn.NothingType).toList
      case tr: Literal =>
        /*
        if literalTypes.map(checkSubtype(tr, _)).contains(None)
        then Nil
        else Some(NotSubtype(tr.tpe, literalTypes.reduce(OrType(_, _)), tr))
        */
        Nil
      case _ => Nil
    ret.iterator.to(List)

// LSP: Check statement types (Any)
object LSPStatements extends Check with MethodsLSP:
  protected def checkAny(term: TermTree)(using tree: Tree)(using Context): Option[NotSubtype] =
    checkSubtype(term.tpe, defn.AnyType)

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

// PseudoLSP: Check type invariants
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
        then checkSubtype(asMethodType(fun).resultType, tr)
        else None
      case tr @ New(tpt) =>
        checkSubtype(tpt.toType, tr)
      case tr @ Typed(expr, tpt) =>
        checkSubtype(tpt.toType, tr)
      case tr @ NamedArg(_, arg) =>
        checkSubtype(arg, tr)
      case tr @ Block(_, expr) =>
        checkSubtype(expr, tr)
      case tr @ If(_, thenPart, elsePart) =>
        checkSubtype(OrType(thenPart.tpe, elsePart.tpe), tr)
      case tr @ InlineIf(_, thenPart, elsePart) =>
        checkSubtype(OrType(thenPart.tpe, elsePart.tpe), tr)
      case tr @ Lambda(meth, tpt) => // REDO
        /*
        for
          t <- tpt
          p <- checkSubtype(t.toType, tr)
        yield p
        // checkSubtype(meth, tr)
        */
        Nil
      case tr @ Match(_, cases) =>
        checkSubtype(cases.map(_.body.tpe).reduce(OrType(_, _)), tr)
      case tr @ InlineMatch(_, cases) =>
        checkSubtype(cases.map(_.body.tpe).reduce(OrType(_, _)), tr)
      case tr @ SeqLiteral(elems, elemtpt) =>
        checkSubtype(defn.SeqTypeOf(elemtpt.toType), tr)
      case tr @ Try(expr, cases, _) =>
        checkSubtype(cases.map(_.body.tpe).fold(expr.tpe)(OrType(_, _)), tr)
      case tr @ Inlined(expr, _, _) =>
        checkSubtype(expr, tr)
      case tr @ Literal(constant) =>
        checkSubtype(constant.wideType, tr) // .tpe.widen
      case _ => Nil
    ret.iterator.to(List)

// PseudoLSP: Check type invariants wihin matchings
object PseudoLSPMatching extends Check with MethodsLSP:
  private def checkPattern(pattern: PatternTree)(using Tree)(using Context): List[NotSubtype] =
    def rec(pat: PatternTree): (Type, List[NotSubtype]) = pat match
      case Bind(_, body, symbol) =>
        val (tpe, problems) = rec(body)
        (tpe, problems ++ checkSubtype(tpe, symbol.declaredType.widen))
      case Alternative(trees) =>
        val (tpes, problems) = trees.map(rec(_)).unzip
        (tpes.reduce(OrType(_, _)), problems.flatten)
      case ExprPattern(expr) =>
        (expr.tpe, Nil)
      case WildcardPattern(tpe) =>
        (tpe, Nil)
      case TypeTest(body, tpt) =>
        (tpt.toType, rec(body)._2)
      case Unapply(fun, implicits, patterns) =>
        def getMember(tpe: Type, name: TermName)(using Context): Option[TermSymbol] =
          try Some(TermRef(tpe, name).symbol)
          catch case _: MemberNotFoundException => None
        
        var localProblems = scala.collection.mutable.ListBuffer.empty[NotSubtype]
        var result = asMethodType(fun).resultType

        if !implicits.isEmpty then
          val fun = result.asInstanceOf[MethodType]
          localProblems.addAll{
            for
              (a, t) <- implicits.zip(fun.paramTypes)
              p <- checkSubtype(a, t)
            yield p
          }
          result = fun.resultType
        
        val productType = TypeRef(defn.scalaPackage.packageRef, typeName("Product"))
        if result.isSubtype(productType) then
          val (tpes, problems) = patterns.map(rec(_)).unzip

          val ftpes = LazyList.from(1)
            .map(x => getMember(result, termName(s"_${x}")))
            .takeWhile(_.isDefined).map(_.get.declaredType)

          localProblems.addAll(problems.flatten)
          /*
          localProblems.addAll{
            for
              (t1, t2) <- tpes.zip(ftpes)
              p <- checkSubtype(t1, t2)
            yield p
          }
          */
        // TODO: Handle boolean and structure { isEmpty, get } cases
        (asMethodType(fun).paramTypes(0), localProblems.to(List))
      rec(pattern)._2
        

  override def check(tree: Tree)(using Context): List[NotSubtype] =
    given Tree = tree
    val ret = tree match
      case Match(selector, cases) =>
        for
          c <- cases.map(_.pattern)
          p <- checkPattern(c) // .widen
        yield p
      case InlineMatch(selector, cases) =>
        for
          s <- selector.toList
          c <- cases.map(_.pattern)
          p <- checkPattern(c) // .widen
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
          (a, t) <- args.zip(fun.tpe.widen.asInstanceOf[PolyType].paramTypeBounds)
          p <- checkBounds(a, t)
        yield p
      case _ => Nil
