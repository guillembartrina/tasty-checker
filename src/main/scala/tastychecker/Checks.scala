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
      ExprTypeConformance,
      ExprTypeRules,
      TypeBoundsConformance
    ).map(x => (x.name, x)).toMap

  def checks(names: List[String]): List[Check] =
    for
      n <- names
      c <- _checks.get(n)
    yield c

  def allChecks: List[Check] = _checks.values.toList


// -------------------------------------------------------
// ExprTypeConformance: Checks that expressions conform to the expected type
object ExprTypeConformance extends Check:
  protected def conformsType(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotConformsType] =
    if term.tpe.isSubtype(tpe) then None else Some(NotConformsType(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotConformsType] =
    given Tree = tree
    val ret = tree match
      case Template(_, _, _, body) =>  //Necessary?
        for case t: TermTree <- body; p <- conformsType(t, defn.AnyType) yield p
      case CaseDef(_, guard, _) =>
        for g <- guard; p <- conformsType(g, defn.BooleanType) yield p
      case ValDef(_, tpt, rhs, _) =>
        for r <- rhs; p <- conformsType(r, tpt.toType) yield p
      case DefDef(_, _, resultTpt, rhs, _) =>
        for r <- rhs; p <- conformsType(r, resultTpt.toType) yield p
      case Apply(fun, args) =>
        val instMeth = fun.tpe.widen.asInstanceOf[MethodType]  //TODO: .instantiate(args.map(_.tpe)).widen.asInstanceOf[MethodType]
        def cleanParamType(tpe: Type): Type = tpe match
          case tpe: ByNameType => tpe.underlying
          case _ => tpe
        val paramTypes = instMeth.paramTypes.map(cleanParamType)
        for (a, tpe) <- args.zip(paramTypes); p <- conformsType(a, tpe) yield p
      case Assign(lhs, rhs) =>
        conformsType(rhs, lhs.tpe.widen)
      case Block(stats, _) =>  //Necessary?
        for case t: TermTree <- stats; p <- conformsType(t, defn.AnyType) yield p
      case If(cond, _, _) =>
        conformsType(cond, defn.BooleanType)
      case InlineIf(cond, _, _) =>
        conformsType(cond, defn.BooleanType)
      case InlineMatch(selector, _) =>  //Necessary?
        for s <- selector; p <- conformsType(s, defn.AnyType) yield p  //Future: defn.MatchableType
      /*
      case Lambda(meth, tpt) =>
        def getSAMName(tpe: Type): TermName =
          def classSymbol(tpe: Type): ClassSymbol = tpe.widen match
            case tpe: TypeRef =>
              tpe.optSymbol.orElse(Some(classSymbol(tpe.optAliasedType.get))).get.asClass
            case tpe: TypeProxy =>
              classSymbol(tpe.superType)
          val s = classSymbol(tpe)
          val candidates = s.declarations.filter(x => x.flags.is(Abstract) && x.name != nme.Constructor).map(_.asTerm)
          if candidates.length == 1 then candidates.head.name else termName("apply")  //Special case: PartialFunction
          
        for
          tpt <- tpt;
          tpe = tpt.toType;
          meth = tpe.widen.lookupMember(getSAMName(tpe)).get.widen.asInstanceOf[MethodType]
        do
          println(meth)        
        //p <- conformsType(meth.tpe.widen, getSAMType(tpt.toType.widen)) yield p
        //if tpt.isDefined then println("NAME" + getSAMName(tpt.get.toType))
        Nil
      */
      case Match(selector, _) =>  //Necessary?
        conformsType(selector, defn.AnyType)  //Future: defn.MatchableType
      case Return(expr, from) =>
        val meth = from.declaredType.asInstanceOf[MethodType]
        for e <- expr; p <- conformsType(e, meth.resultType) yield p
      case SeqLiteral(elems, elemtpt) =>
        for e <- elems; p <- conformsType(e, elemtpt.toType) yield p
      case Throw(expr) =>
        conformsType(expr, defn.ThrowableType)
      case Try(expr, _, finalizer) =>  //Necessary?
        conformsType(expr, defn.AnyType) ++ (for f <- finalizer; p <- conformsType(f, defn.UnitType) yield p)
      case Typed(expr, tpt) =>
        val tpe = tpt.toType
        if tpe.isOfClass(defn.RepeatedParamClass)
        then conformsType(expr, defn.SeqTypeOf(tpe.asInstanceOf[AppliedType].args(0)))
        else conformsType(expr, tpe)
      case While(cond, body) =>
        conformsType(cond, defn.BooleanType) ++ conformsType(body, defn.UnitType)
      case _ => Nil
    ret.iterator.to(List)

// -------------------------------------------------------
// ExprTypeRules: Checks that expressions follow the typing rules
object ExprTypeRules extends Check:
  protected def matchesType(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotMatchesType] =
    if term.tpe.isSameType(tpe) then None else Some(NotMatchesType(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotMatchesType] =
    given Tree = tree
    val ret = tree match
      case tr @ Apply(fun, args) =>
        @tailrec def isApplyNew(tree: TermTree)(using Context): Boolean = tree match
          case Apply(fn, _) => isApplyNew(fn)
          case TypeApply(fn, _) => isApplyNew(fn)
          case Block(_, expr) => isApplyNew(expr)
          case Select(New(_), SignedName(nme.Constructor, _, _)) => true
          case _ => false
        if !isApplyNew(tr)
        then
          val instResultType = fun.tpe.widen.asInstanceOf[MethodType].instantiate(args.map(_.tpe))
          matchesType(tr, instResultType)
        else None
      case tr @ Assign(_, _) =>
        matchesType(tr, defn.UnitType)
      case tr @ Block(_, expr) =>
        matchesType(tr, expr.tpe)
      case tr @ If(_, thenPart, elsePart) =>
        matchesType(tr, OrType(thenPart.tpe, elsePart.tpe))
      case tr @ InlineIf(_, thenPart, elsePart) =>
        matchesType(tr, OrType(thenPart.tpe, elsePart.tpe))
      case tr @ InlineMatch(_, cases) =>
        matchesType(tr, cases.map(_.body.tpe).reduce(OrType(_, _)))
      case tr @ Ident(_) =>  //???
        matchesType(tr, tr.tpe)
      case tr @ Inlined(expr, _, _) =>
        matchesType(tr, expr.tpe)
      case tr @ Lambda(meth, tpt) =>
        if tpt.isDefined
        then matchesType(tr, tpt.get.toType)
        else None  //TODO
      case tr @ Literal(constant) =>
        matchesType(tr, ConstantType(constant))
      case tr @ Match(_, cases) =>
        matchesType(tr, cases.map(_.body.tpe).reduce(OrType(_, _)))
      case tr @ NamedArg(_, arg) =>
        matchesType(tr, arg.tpe)
      case tr @ New(tpt) =>
        matchesType(tr, tpt.toType)
      case tr @ Return(_, _) =>
        matchesType(tr, defn.NothingType)
      /*
      case tr @ Select(qualifier, name) =>  //???
        matchesType()
      */
      case tr @ SeqLiteral(_, elemtpt) =>
        matchesType(tr, defn.SeqTypeOf(elemtpt.toType))
      /*
      case tr @ Super(qual, mix) =>  //???
        matchesType(tr, SuperType(qual.tpe.asInstanceOf[ThisType].cls, mix.map(_.toType)))
      */
      case tr @ This(qualifier) =>  //???
        qualifier.toType match
          case pkg: PackageRef => matchesType(tr, pkg)
          case tpe: TypeRef => matchesType(tr, ThisType(tpe))
          case _ => None
      case tr @ Throw(_) =>
        matchesType(tr, defn.NothingType)
      case tr @ Try(expr, cases, _) =>
        matchesType(tr, cases.map(_.body.tpe).foldLeft(expr.tpe)(OrType(_, _)))
      case tr @ Typed(_, tpt) =>
        matchesType(tr, tpt.toType)
      case tr @ While(_, _) =>
        matchesType(tr, defn.UnitType)
      case _ => Nil
    ret.iterator.to(List)

/*
// REDO
// PatternTypeRules: Checks that patterns in matchings follow the typing rules
object PatternTypeRules extends Check:
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
*/

// -------------------------------------------------------
// TypeBoundsConformance: Checks that type parameters conform to bounds
object TypeBoundsConformance extends Check:
  private def checkBounds(tpe: TypeTree, bounds: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsBounds] =
    if bounds.contains(tpe.toType) then None else Some(NotConformsBounds(tpe.toType, bounds, tree))

  override def check(tree: Tree)(using Context): List[NotConformsBounds] =
    given Tree = tree
    tree match
      case TypeApply(fun, args) =>
        for
          (a, b) <- args.zip(fun.tpe.widen.asInstanceOf[PolyType].paramTypeBounds)
          p <- checkBounds(a, b)
        yield p
      case _ => Nil
