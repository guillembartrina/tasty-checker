package tastychecker

import scala.annotation.tailrec

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Flags.*
import tastyquery.Symbols.*
import tastyquery.Exceptions.*
import tastyquery.Trees.*
import tastyquery.Types.*

import tastyquery.Erasure.erase

abstract class Check:
  val name: String = getClass.getSimpleName.init
  def check(tree: Tree)(using Context): List[Problem]

object Check:
  private val _checks: Map[String, Check] =
    List(
      ExprTypeConformance,
      ExprTypeRules,
      MatchingTypeRules,
      TypeBoundsConformance,
      TypeMemberBoundsConformance,
      MemberOverridingTypeConformance,
      MemberOverridingRules
    ).map(x => (x.name, x)).toMap

  def checks(names: List[String]): List[Check] =
    for n <- names; c <- _checks.get(n) yield c

  def allChecks: List[Check] = _checks.values.toList


// -------------------------------------------------------
// ExprTypeConformance: Checks that expressions' type conforms to the expected type
object ExprTypeConformance extends Check:
  private def conformsType(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotConformsType] =
    if term.tpe.isSubtype(tpe) then None else Some(NotConformsType(term.tpe, tpe, tree))

  override def check(tree: Tree)(using Context): List[NotConformsType] =
    given Tree = tree
    val ret = tree match
      case Template(_, _, _, body) =>
        for case t: TermTree <- body; p <- conformsType(t, defn.AnyType) yield p
      case CaseDef(_, guard, _) =>
        for g <- guard; p <- conformsType(g, defn.BooleanType) yield p
      case ValDef(_, tpt, rhs, _) =>
        for r <- rhs; p <- conformsType(r, tpt.toType) yield p
      case DefDef(_, _, resultTpt, rhs, _) =>
        for r <- rhs; p <- conformsType(r, resultTpt.toType) yield p
      case Apply(fun, args) =>
        val meth = fun.tpe.widen.asInstanceOf[MethodType]
        def cleanParamType(tpe: Type): Type = tpe match
          case tpe: ByNameType => tpe.underlying
          case _ => tpe
        val paramTypes = meth.instantiateParamTypes(args.map(_.tpe)).map(cleanParamType)
        for (a, tpe) <- args.zip(paramTypes); p <- conformsType(a, tpe) yield p
      case Assign(lhs, rhs) =>
        conformsType(rhs, lhs.tpe.widen)
      case Block(stats, _) =>
        for case t: TermTree <- stats; p <- conformsType(t, defn.AnyType) yield p
      case If(cond, _, _) =>
        conformsType(cond, defn.BooleanType)
      case InlineIf(cond, _, _) =>
        conformsType(cond, defn.BooleanType)
      case InlineMatch(selector, _) =>
        for s <- selector; p <- conformsType(s, defn.AnyType) yield p  //Future: defn.MatchableType
      case Lambda(meth, tpt) =>
        val partialFunctionClass = defn.scalaPackage.getDecl(typeName("PartialFunction")).get.asClass
        def getSAM(tpe: Type): TermSymbol =
          def classSymbol(tpe: Type): ClassSymbol = tpe.widen match
            case tpe: TypeRef => tpe.optSymbol.orElse(Some(classSymbol(tpe.optAliasedType.get))).get.asClass
            case tpe: TypeProxy => classSymbol(tpe.superType)
            case _ => ???
          if tpe.isOfClass(partialFunctionClass)  //Special case: PartialFunction
          then partialFunctionClass.parentClasses.last.getNonOverloadedDecl(termName("apply")).get
          else classSymbol(tpe).declarations.filter(x => x.flags.is(Abstract) && x.name != nme.Constructor).head.asTerm
        if tpt.isDefined
        then
          val tpe = tpt.get.toType
          val nmeth = meth.tpe.widen.asInstanceOf[MethodType]
          val sam = tpe.widen.select(getSAM(tpe)).widen.asInstanceOf[MethodType]
          if !nmeth.resultType.isSubtype(sam.instantiate(nmeth.paramTypes))
             || sam.paramTypes.zip(nmeth.paramTypes).exists((s, a) => !s.isSubtype(a))
          then Some(NotConformsType(nmeth, sam, tree))
          else None
        else None
      case Match(selector, _) =>
        conformsType(selector, defn.AnyType)  //Future: defn.MatchableType
      case Return(expr, from) =>
        val meth = from.declaredType.asInstanceOf[MethodType]
        for e <- expr; p <- conformsType(e, meth.resultType) yield p
      case SeqLiteral(elems, elemtpt) =>
        for e <- elems; p <- conformsType(e, elemtpt.toType) yield p
      case Throw(expr) =>
        conformsType(expr, defn.ThrowableType)
      case Try(expr, _, finalizer) =>
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
// ExprTypeRules: Checks that expressions' type follows the typing rules
object ExprTypeRules extends Check:
  private def matchesType(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotMatchesType] =
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
        else None  //TODO (?)
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
      // case Ident
      case tr @ Inlined(expr, _, _) =>
        matchesType(tr, expr.tpe)
      case tr @ Lambda(meth, tpt) =>
        if tpt.isDefined
        then matchesType(tr, tpt.get.toType)
        else None  // TODO (?)
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
      // case Select
      case tr @ SeqLiteral(_, elemtpt) =>
        matchesType(tr, defn.SeqTypeOf(elemtpt.toType))
      // case Super
      case tr @ This(qualifier) =>
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

// MatchingTypeRules: Checks that matchings follows the typing rules
object MatchingTypeRules extends Check:
  private def matchesType(tpea: Type, tpeb: Type)(using tree: Tree)(using Context): Option[NotMatchesType] =
    if tpea.isSameType(tpeb) then None else Some(NotMatchesType(tpea, tpeb, tree))

  private def checkPattern(sel: Type, pattern: PatternTree)(using Tree)(using Context): List[NotMatchesType] =
    def rec(sel: Type, pat: PatternTree): (Type, List[NotMatchesType]) =
      given Tree = pat
      pat match
        case Bind(_, body, symbol) =>
          val (tpe, problems) = rec(sel, body)
          (TermRef(NoPrefix, symbol), problems ++ matchesType(tpe, symbol.declaredType))
        case Alternative(trees) =>
          val (tpes, problems) = trees.map(rec(sel, _)).unzip
          (tpes.reduce(OrType(_, _)), problems.flatten)
        case ExprPattern(_) =>
          (sel, Nil)
        case WildcardPattern(tpe) =>
          (tpe, matchesType(tpe, sel).toList)
        case TypeTest(body, tpt) =>
          val tpe = if tpt.toType.isOfClass(defn.RepeatedParamClass)
            then defn.SeqTypeOf(tpt.toType.asInstanceOf[AppliedType].args(0))
            else tpt.toType
          val (_, problems) = rec(tpe, body)
          (AndType(tpt.toType, sel), problems)
        case Unapply(fun, implicits, patterns) =>
          def funTermName(term: TermTree): TermName = term match
            case Select(_, SignedName(termName, _, _)) => termName
            case TypeApply(fun, _) => funTermName(fun)
            case _ => ???
          if patterns.isEmpty || funTermName(fun) == SimpleName("unapplySeq")
          then (null, Nil)
          else
            def getMember(tpe: Type, name: TermName): Option[TermSymbol] =  //IMPROVE?
              try Some(TermRef(tpe, name).symbol)
              catch case _: MemberNotFoundException => None
            def get_NMemberTypes(tpe: Type): List[Type] =
              LazyList.from(1).map(x => getMember(tpe, termName(s"_${x}")))
                .takeWhile(_.isDefined).map(_.get.declaredType).toList
            var res = fun.tpe.widen.asInstanceOf[MethodType].instantiate(List(sel))
            if !implicits.isEmpty then res = res.widen.asInstanceOf[MethodType].instantiate(implicits.map(_.tpe))
            if res.isSubtype(TypeRef(defn.scalaPackage.packageRef, typeName("Product")))
            then
              val tpes = get_NMemberTypes(res)
              (null, patterns.zip(tpes).map((p, t) => checkPattern(t, p)).flatten)
            else
              val tpe = getMember(res, termName("get")).get.declaredType
              if patterns.size == 1
              then (null, checkPattern(tpe, patterns(0)))
              else
                val tpes = get_NMemberTypes(tpe)
                (null, patterns.zip(tpes).map((p, t) => checkPattern(t, p)).flatten)
    rec(sel, pattern)._2

  override def check(tree: Tree)(using Context): List[NotMatchesType] =
    given Tree = tree
    val ret = tree match
      case Match(selector, cases) =>
        for c <- cases.map(_.pattern); p <- checkPattern(selector.tpe.widen, c) yield p
      case InlineMatch(selector, cases) =>
        for s <- selector.toList; c <- cases.map(_.pattern); p <- checkPattern(s.tpe.widen, c) yield p  // Why toList??
      case Try(_, cases, _) =>
        for c <- cases.map(_.pattern); p <- checkPattern(defn.ThrowableType, c) yield p
      case _ => Nil
    ret.iterator.to(List)

// -------------------------------------------------------
// TypeBoundsConformance: Checks that type parameters' type conform to bounds
object TypeBoundsConformance extends Check:
  private def conformsBounds(tpe: TypeTree, bounds: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsBounds] =
    if bounds.contains(tpe.toType) then None else Some(NotConformsBounds(tpe.toType, bounds, tree))

  override def check(tree: Tree)(using Context): List[NotConformsBounds] =
    given Tree = tree
    tree match
      case TypeApply(fun, args) =>
        for (a, b) <- args.zip(fun.tpe.widen.asInstanceOf[PolyType].paramTypeBounds); p <- conformsBounds(a, b) yield p
      case AppliedTypeTree(tycon, args) =>
        for (a, b) <- args.zip(tycon.toType.typeParams.map(_.bounds)); p <- conformsBounds(a, b) yield p
      case _ => Nil

// -------------------------------------------------------
// TypeMemberBoundsConformance: Checks that type members' bounds conform to the overriden bounds
object TypeMemberBoundsConformance extends Check:
  private def conformsBounds(boundsa: TypeBounds, boundsb: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsBounds2] =
    if boundsb.contains(boundsa) then None else Some(NotConformsBounds2(boundsa, boundsb, tree))

  override def check(tree: Tree)(using Context): List[NotConformsBounds2] =
    given Tree = tree
    tree match
      case TypeMember(_, _, symbol) =>
        def mapBounds(bounds: TypeBounds, f: Type => Type): TypeBounds =  //TEMPORAL
          bounds match
            case RealTypeBounds(low, high) => RealTypeBounds(f(low), f(high))
            case TypeAlias(alias) => TypeAlias(f(alias))
        for
          s <- symbol.nextOverriddenSymbol.toList
          b = mapBounds(s.asInstanceOf[TypeMemberSymbol].bounds, _.asSeenFrom(symbol.owner.asClass.thisType, s.owner))
          p <- conformsBounds(symbol.bounds, b)
        yield p
      case _ => Nil

// -------------------------------------------------------
// MemberOverridingTypeConformance: Checks that member' types conform to the overriden type
object MemberOverridingTypeConformance extends Check:
  private def conformsType(tpea: Type, tpeb: Type)(using tree: Tree)(using Context): Option[NotConformsType] =
    if tpea.isSubtype(tpeb) then None else Some(NotConformsType(tpea, tpeb, tree))

  override def check(tree: Tree)(using Context): List[NotConformsType] =
    given Tree = tree
    tree match
      case ValDef(_, _, _, symbol) =>
        for
          s <- symbol.nextOverriddenSymbol.toList
          tpe = s.asTerm.declaredType.asSeenFrom(symbol.owner.asClass.thisType, s.owner)
          p <- conformsType(symbol.declaredType, tpe)
        yield p
      case DefDef(_, _, _, _, symbol) =>
        def stripParameterlessMethod(tpe: Type): Type = tpe match
          case meth: MethodType if meth.paramNames.isEmpty => meth.resultType
          case _ => tpe
        for
          s <- symbol.nextOverriddenSymbol.toList
          tpe = stripParameterlessMethod(s.asTerm.declaredType).asSeenFrom(symbol.owner.asClass.thisType, s.owner)
          p <- conformsType(stripParameterlessMethod(symbol.declaredType), tpe)
        yield p
      case _ => Nil

// -------------------------------------------------------
// MemberOverridingRules: Checks that members with the same erasure indeed override
object MemberOverridingRules extends Check:
  private def overrides(sa: TermSymbol, sb: TermSymbol)(using tree: Tree)(using Context): Option[NotOverrides] =
    if sa.nextOverriddenSymbol.map(s => s == sb).getOrElse(false) then None else Some(NotOverrides(sa, sb, tree))

  private def erase2(tpe: Type)(using Context): Tuple = tpe match
    case tpe: MethodType => tpe.paramTypes.map(erase2(_)) *: erase2(tpe.resultType) *: EmptyTuple
    case tpe: PolyType => erase2(tpe.resultType) *: EmptyTuple
    case tpe: AndType => EmptyTuple
    case tpe => 
      try erase(tpe) *: EmptyTuple
      catch case _: UnsupportedOperationException => EmptyTuple

  private def findCandidate(target: TermSymbol)(using Context): Option[TermSymbol] =
    if target.owner.isClass && !target.is(Private) && target.name != nme.Constructor then
      val targetType = erase2(target.declaredType)
      val thisType = target.owner.asClass.thisType
      def candidate(in: ClassSymbol): Option[TermSymbol] =
        val candidates = in.getAllOverloadedDecls(target.name).filterNot(_.is(Private))
        candidates.find(c => erase2(c.declaredType.asSeenFrom(thisType, c.owner)) == targetType)
      val ancestors = target.owner.asClass.linearization.tail
      ancestors.iterator.map(candidate(_)).flatten.nextOption()
    else None

  override def check(tree: Tree)(using Context): List[NotOverrides] =
    given Tree = tree
    tree match
      case ValDef(_, _, _, symbol) =>
        for c <- findCandidate(symbol).toList; p <- overrides(symbol, c) yield p
      case DefDef(_, _, _, _, symbol) =>
        for c <- findCandidate(symbol).toList; p <- overrides(symbol, c) yield p
      case _ => Nil
