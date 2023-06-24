package tastychecker

import scala.annotation.tailrec

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Flags.*
import tastyquery.Signatures.*
import tastyquery.SourceLanguage
import tastyquery.Symbols.*
import tastyquery.Exceptions.*
import tastyquery.Trees.*
import tastyquery.Types.*


type TreeCheck = Check[Tree]

object TreeCheck:
  val allChecks: List[TreeCheck] = List(
    ExprTypeConformance,
    MatchingTypeCorrectness,
    TypeBoundsConformance,
    TypeMemberOverridingBoundsConformance,
    MemberOverridingTypeConformance,
    MemberErasureOverridance,
    LocalReferencesScoping,

    ExprTypeRules  //Not really a check...
  )
  def someChecks(names: List[String]): List[TreeCheck] =
    for n <- names; c <- allChecks.find(_.name == n) yield c

// ----------

private abstract class Check[T: Treelike]:
  val name: String = getClass.getSimpleName.init
  def check(t: T)(using Filter[T])(using Context): List[Problem]

private object Check:
  def tryMerge[T: Treelike](checks: List[Check[T]]): List[Check[T]] =
    val (contextless, other) = checks.partitionMap{
      case cl: ContextlessCheck[?] => Left(cl)
      case o => Right(o)
    }
    ContextlessCheck.tryMerge(contextless) ::: other

private abstract class ContextlessCheck[T: Treelike] extends Check[T]:
  override def check(t: T)(using filter: Filter[T])(using Context): List[Problem] =
    (for tp <- walkT(t) if !filter.matches(tp); p <- checkT(tp) yield p).toList
  protected def walkT(t: T)(using Context): Iterator[T]
  protected def checkT(t: T)(using Context): List[Problem]

private object ContextlessCheck:
  trait FullPreorderWalk[T: Treelike]:
    protected def walkT(t: T)(using Context): Iterator[T] =
      Iterator.single(t) ++ (for st <- t.children.iterator; tp <- walkT(st) yield tp)
  
  def tryMerge[T: Treelike](checks: List[ContextlessCheck[T]]): List[ContextlessCheck[T]] =
    val (fullPreorder, other) = checks.partitionMap{
      case fpw: FullPreorderWalk[?] => Left(fpw)
      case o => Right(o)
    }
    Option.when(fullPreorder.nonEmpty){
      object Anon extends ContextlessCheck[T] with FullPreorderWalk[T]:
        override protected def checkT(t: T)(using Context): List[Problem] =
          fullPreorder.flatMap(_.checkT(t))
      Anon
    }.toList ::: other


private abstract class ContextfulCheck[T: Treelike, C] extends Check[T]:
  override def check(t: T)(using filter: Filter[T])(using Context): List[Problem] =
    (for (tp, cp) <- walkT(t, initialContext) if !filter.matches(tp); p <- checkT(tp, cp) yield p).toList
  protected def initialContext: C
  protected def walkT(t: T, context: C)(using Context): Iterator[(T, C)]
  protected def checkT(t: T, context: C)(using Context): List[Problem]

private object ContextfulCheck:
  trait CustomWalkBySteps[T: Treelike, C]:
    protected def walkT(t: T, context: C)(using Context): Iterator[(T, C)] =
      Iterator.single((t, context)) ++ (for (st, sc) <- step(t, context).iterator; (tp, cp) <- walkT(st, sc) yield (tp, cp))
    protected def step(t: T, context: C)(using Context): List[(T, C)]


// -------------------------------------------------------
// ExprTypeConformance: Checks that expressions' types conform to the expected type
private object ExprTypeConformance extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
  private def conformsType(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotConformsType] =
    if term.tpe.isSubtype(tpe) then None else Some(NotConformsType(term.tpe, tpe, tree))

  override protected def checkT(tree: Tree)(using Context): List[NotConformsType] =
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
    ret.iterator.toList

// -------------------------------------------------------
// MatchingTypeCorrectness: Checks that matchings' patterns have the correct types
private object MatchingTypeCorrectness extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
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
            def getMember(tpe: Type, name: TermName): Option[TermSymbol] =
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

  override protected def checkT(tree: Tree)(using Context): List[NotMatchesType] =
    given Tree = tree
    tree match
      case Match(selector, cases) =>
        for c <- cases.map(_.pattern); p <- checkPattern(selector.tpe.widen, c) yield p
      case InlineMatch(selector, cases) =>
        for s <- selector.toList; c <- cases.map(_.pattern); p <- checkPattern(s.tpe.widen, c) yield p
      case Try(_, cases, _) =>
        for c <- cases.map(_.pattern); p <- checkPattern(defn.ThrowableType, c) yield p
      case _ => Nil

// -------------------------------------------------------
// TypeBoundsConformance: Checks that type parameters' types conform to bounds
private object TypeBoundsConformance extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
  private def conformsBounds(tpe: TypeTree, bounds: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsBounds] =
    if bounds.contains(tpe.toType) then None else Some(NotConformsBounds(tpe.toType, bounds, tree))

  private object _TypeCheck extends ContextlessCheck[Type] with ContextlessCheck.FullPreorderWalk[Type]:
    private def conformsBounds(tpe: Type, bounds: TypeBounds)(using tpe2: Type)(using Context): Option[NotConformsBounds] =
      if bounds.contains(tpe) then None else Some(NotConformsBounds(tpe, bounds, tpe2))

    override protected def checkT(tpe: Type)(using Context): List[NotConformsBounds] =
      given Type = tpe
      tpe match
        case tpe: AppliedType =>
          for (a, b) <- tpe.args.zip(tpe.tycon.typeParams.map(_.bounds)); p <- conformsBounds(a, b) yield p
        case _ => Nil

  override protected def checkT(tree: Tree)(using Context): List[NotConformsBounds] =
    given Tree = tree
    val tp = for tpe <- portalTreeToType(tree); p <- _TypeCheck.check(tpe)(using Filter.empty[Type]) yield p.asInstanceOf[NotConformsBounds]
    val ret = tree match
      case TypeApply(fun, args) =>
        for (a, b) <- args.zip(fun.tpe.widen.asInstanceOf[PolyType].paramTypeBounds); p <- conformsBounds(a, b) yield p
      case _ => Nil
    tp ::: ret

// -------------------------------------------------------
// TypeMemberBoundsConformance: Checks that type members' bounds conform to overridden bounds
private object TypeMemberOverridingBoundsConformance extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
  private def conformsBounds(boundsa: TypeBounds, boundsb: TypeBounds)(using tree: Tree)(using Context): Option[NotConformsBounds] =
    if boundsb.contains(boundsa) then None else Some(NotConformsBounds(boundsa, boundsb, tree))

  override protected def checkT(tree: Tree)(using Context): List[NotConformsBounds] =
    given Tree = tree
    tree match
      case TypeMember(_, _, symbol) =>
        for
          s <- symbol.allOverriddenSymbols.toList
          b = s.asInstanceOf[TypeMemberSymbol].bounds.mapBounds(_.asSeenFrom(symbol.owner.asClass.thisType, s.owner))
          p <- conformsBounds(symbol.bounds, b)
        yield p
      case _ => Nil

// -------------------------------------------------------
// MemberOverridingTypeConformance: Checks that member' types conform to overridden type
private object MemberOverridingTypeConformance extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
  private def conformsType(tpea: Type, tpeb: Type)(using tree: Tree)(using Context): Option[NotConformsType] =
    if tpea.isSubtype(tpeb) then None else Some(NotConformsType(tpea, tpeb, tree))

  override protected def checkT(tree: Tree)(using Context): List[NotConformsType] =
    def helper(symbol: TermSymbol): List[NotConformsType] =
      def methodize(tpe: Type): Type = if !tpe.isInstanceOf[MethodType] then MethodType(Nil, Nil, tpe) else tpe
      val ojm = symbol.allOverriddenSymbols.find(_.asTerm.sourceLanguage == SourceLanguage.Java).isDefined
      for
        s <- symbol.allOverriddenSymbols.toList
        tpea = if ojm then methodize(symbol.declaredType) else symbol.declaredType
        tpeb = if ojm then methodize(s.asTerm.declaredType) else s.asTerm.declaredType
        tpebp = tpeb.asSeenFrom(symbol.owner.asClass.thisType, s.owner)
        p <- conformsType(tpea, tpebp)
      yield p
    given Tree = tree
    tree match
      case ValDef(_, _, _, symbol) => helper(symbol)
      case DefDef(_, _, _, _, symbol) => helper(symbol)
      case _ => Nil

// -------------------------------------------------------
// MemberErasureOverridance: Checks that members with the same erasure indeed override
private object MemberErasureOverridance extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
  private def overrides(sa: TermSymbol, sb: TermSymbol)(using tree: Tree)(using Context): Option[NotOverrides] =
    if sa.allOverriddenSymbols.exists(_ == sb) then None else Some(NotOverrides(sa, sb, tree))

  private def findCandidates(target: TermSymbol)(using Context): List[TermSymbol] =
    if target.owner.isClass && !target.is(Private) && target.name != nme.Constructor then
      val thisType = target.owner.asClass.thisType
      def candidate(in: ClassSymbol): Option[TermSymbol] =
        val candidates = in.getAllOverloadedDecls(target.name).filterNot(_.is(Private)).filter(_.targetName == target.targetName)
        candidates.find(c => c.declaredType.asSeenFrom(thisType, c.owner).signature(c.sourceLanguage) == target.signature)
      for p <- target.owner.asClass.parentClasses; c <- candidate(p) yield c
    else Nil

  override protected def checkT(tree: Tree)(using Context): List[NotOverrides] =
    given Tree = tree
    tree match
      case ValDef(_, _, _, symbol) =>
        for c <- findCandidates(symbol).toList; p <- overrides(symbol, c) yield p
      case DefDef(_, _, _, _, symbol) =>
        for c <- findCandidates(symbol).toList; p <- overrides(symbol, c) yield p
      case _ => Nil

// -------------------------------------------------------
// LocalReferencesScoping: Checks that local references are in scope
private case class Scope(terms: Set[TermSymbol], types: Set[TypeSymbol]):
  def withTerm(ts: TermSymbol): Scope = copy(terms = terms + ts)
  def withTerms(tss: List[TermSymbol]): Scope = copy(terms = terms ++ tss)
  def withType(ts: TypeSymbol): Scope = copy(types = types + ts)
  def withTypes(tss: List[TypeSymbol]): Scope = copy(types = types ++ tss)

private object Scope:
  def empty: Scope = Scope(Set.empty[TermSymbol], Set.empty[TypeSymbol])

private object LocalReferencesScoping extends ContextfulCheck[Tree, Scope] with ContextfulCheck.CustomWalkBySteps[Tree, Scope]:
  override protected def initialContext: Scope = Scope.empty

  override protected def step(tree: Tree, context: Scope)(using Context): List[(Tree, Scope)] =  //PRUNE BRANCHES?
    tree match
      case Block(stats, expr) =>
        (stats :+ expr).foldLeft((context, List.empty[(Tree, Scope)])){ case ((s, l), t) =>
          val ns = t match
            case ValDef(_, _, _, symbol) => s.withTerm(symbol)
            case DefDef(_, _, _, _, symbol) => s.withTerm(symbol)
            case ClassDef(_, _, symbol) => s.withType(symbol)
            case TypeMember(_, _, symbol) => s.withType(symbol)
            case _ => s
          (ns, (t, ns) :: l)
        }._2
      case Inlined(expr, _, bindings) =>
        (bindings :+ expr).foldLeft((context, List.empty[(Tree, Scope)])){ case ((s, l), t) =>
          val ns = t match
            case ValDef(_, _, _, symbol) => s.withTerm(symbol)
            case DefDef(_, _, _, _, symbol) => s.withTerm(symbol)
            case _ => s
          (ns, (t, ns) :: l)
        }._2
      case DefDef(_, paramList, resultTpt, rhs, _) =>
        val (nc, ps) = paramList.foldLeft((context, List.empty[(Tree, Scope)])){ case ((s, l), c) =>
          c match
            case Left(valDefs) =>
              valDefs.foldLeft((s, l)){ case ((s2, l2), valDef) =>
                (s2.withTerm(valDef.symbol), (valDef, s2.withTerm(valDef.symbol)) :: l2)
              }
            case Right(typeParams) =>
              (s.withTypes(typeParams.map(_.symbol)), typeParams.map((_, s.withTypes(typeParams.map(_.symbol)))) ::: l)
        }
        (resultTpt, nc) :: rhs.map((_, nc)).toList ::: ps
      case TypeLambdaTree(tparams, body) =>
        val nc = context.withTypes(tparams.map(_.symbol))
        (body, nc) :: tparams.map((_, nc))
      case PolyTypeDefinitionTree(tparams, body) =>
        val nc = context.withTypes(tparams.map(_.symbol))
        (body, nc) :: tparams.map((_, nc))
      case CaseDef(pattern, guard, body) =>
        /*
        def getTypeBindings(tpe: Type): List[TypeSymbol] = tpe match
          case tpe: TypeRef if tpe.prefix == NoPrefix => tpe.optSymbol.toList
          case tpe => tpe.children.flatMap(getTypeBindings(_))
        */
        def collectBinds(tree: Tree): List[TermOrTypeSymbol] =
          val s = tree match
            case Bind(_, _, symbol) => symbol :: Nil
            case TypeTreeBind(_, _, symbol) => symbol :: Nil
            case TypeBindingsTree(bindings, _) => bindings.map(_.symbol)
            case _ => Nil
          s ::: tree.children.flatMap(collectBinds(_)) //::: portalTreeToType(tree).flatMap(getTypeBindings(_))
        val (terms, types) = collectBinds(pattern).partitionMap{
          case s: TermSymbol => Left(s)
          case s: TypeSymbol => Right(s)
        }
        val nc = context.withTerms(terms).withTypes(types)
        (pattern, nc) :: (body, nc) :: guard.map((_, nc)).toList
      case TypeCaseDef(pattern, body) =>
        def collectBinds(patt: TypeTree): List[TypeSymbol] = patt match
          case TypeTreeBind(_, _, symbol) => symbol :: Nil
          case AppliedTypeTree(_, targs) => targs.flatMap(collectBinds(_))
          case _ => Nil
        val nc = context.withTypes(collectBinds(pattern))
        (pattern, nc) :: (body, nc) :: Nil
      case RefinedTypeTree(underlying, refinements, refinedCls) =>
        (underlying, context) :: refinements.map((_, context.withType(refinedCls)))
      case _ => tree.children.map((_, context))

  private class _TypeCheck(var scope: Scope) extends ContextlessCheck[Type] with ContextlessCheck.FullPreorderWalk[Type]:
    private def termInScope(ts: TermSymbol, scope: Scope)(using tpe: Type): Option[NotInScope] =
      if scope.terms.contains(ts) then None else Some(NotInScope(ts, tpe))
    private def typeInScope(ts: TypeSymbol, scope: Scope)(using tpe: Type): Option[NotInScope] =
      if scope.types.contains(ts) then None else Some(NotInScope(ts, tpe))
    
    override protected def checkT(tpe: Type)(using Context): List[NotInScope] =
      given Type = tpe
      tpe match
        case tpe: TermRef if tpe.prefix == NoPrefix =>
          termInScope(tpe.symbol, scope).toList
        case tpe: TypeRef if tpe.prefix == NoPrefix =>
          typeInScope(tpe.optSymbol.get, scope).toList
        case _ => Nil
  
  override protected def checkT(tree: Tree, context: Scope)(using Context): List[NotInScope] =
    given Tree = tree
    for tpe <- portalTreeToType(tree); p <- _TypeCheck(context).check(tpe)(using Filter.empty[Type]) yield p.asInstanceOf[NotInScope]



// -------------------------------------------------------
// ExprTypeRules: Checks that expressions' types follow the typing rules
private object ExprTypeRules extends ContextlessCheck[Tree] with ContextlessCheck.FullPreorderWalk[Tree]:
  private def matchesType(term: TermTree, tpe: Type)(using tree: Tree)(using Context): Option[NotMatchesType] =
    if term.tpe.isSameType(tpe) then None else Some(NotMatchesType(term.tpe, tpe, tree))

  override protected def checkT(tree: Tree)(using Context): List[NotMatchesType] =
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
    ret.iterator.toList
