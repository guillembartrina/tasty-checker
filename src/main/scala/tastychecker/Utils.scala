package tastychecker

import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Signatures.*
import tastyquery.SourceLanguage
import tastyquery.Trees.*
import tastyquery.Types.*
import scala.collection.immutable.Stream.Cons


extension (tree: Tree)
  def subtrees: List[Tree] = tree match
    case CaseDef(pattern, guard, body)                          => pattern :: guard.toList ::: body :: Nil
    case ImportIdent(name)                                      => Nil
    case ImportSelector(imported, renamed, bound)               => imported :: renamed.toList ::: bound.toList
    case SelfDef(name, tpt)                                     => tpt :: Nil  //Nil
    case Template(constr, parents, self, body)                  => constr :: parents ::: self.toList ::: body
    case TypeCaseDef(pattern, body)                             => pattern :: body :: Nil

    //- TopLevelTree
    case PackageDef(pid, stats)                                 => stats

    //-- StatementTree
    case Import(expr, selectors)                                => expr :: selectors
    case Export(expr, selectors)                                => expr :: selectors

    //--- ValOrDefDef
    case DefDef(name, paramLists, resultTpt, rhs, symbol)       => paramLists.flatMap(_.merge) ::: resultTpt :: rhs.toList
    case ValDef(name, tpt, rhs, symbol)                         => tpt :: rhs.toList

    //--- TypeDef
    case ClassDef(name, rhs, symbol)                            => rhs :: Nil
    case TypeMember(name, rhs, symbol)                          => rhs :: Nil
    case TypeParam(name, bounds, symbol)                        => bounds :: Nil

    //--- TermTree
    case Apply(fun, args)                                       => fun :: args
    case Assign(lhs, rhs)                                       => lhs :: rhs :: Nil
    case Block(stats, expr)                                     => stats :+ expr
    case Ident(name)                                            => Nil
    case If(cond, thenPart, elsePart)                           => cond :: thenPart :: elsePart :: Nil
    case InlineIf(cond, thenPart, elsePart)                     => cond :: thenPart :: elsePart :: Nil
    case InlineMatch(selector, cases)                           => selector.toList ::: cases
    case Inlined(expr, caller, bindings)                        => expr :: bindings  //expr :: caller.toList ::: bindings
    case Lambda(meth, tpt)                                      => meth :: tpt.toList
    case Literal(constant)                                      => Nil
    case Match(selector, cases)                                 => selector :: cases
    case NamedArg(name, arg)                                    => arg :: Nil
    case New(tpt)                                               => tpt :: Nil
    case Return(expr, from)                                     => expr.toList
    case Select(qualifier, name)                                => qualifier :: Nil
    case SelectOuter(qualifier, levels)                         => qualifier :: Nil
    case SeqLiteral(elems, elempt)                              => elems ::: elempt :: Nil
    case Super(qual, mix)                                       => qual :: Nil  //qual :: mix.toList
    case This(qualifier)                                        => Nil  //qualifier :: Nil
    case Throw(expr)                                            => expr :: Nil
    case Try(expr, cases, finalizer)                            => expr :: cases ::: finalizer.toList
    case TypeApply(fun, args)                                   => fun :: args
    case Typed(expr, tpt)                                       => expr :: tpt :: Nil
    case While(cond, body)                                      => cond :: body :: Nil

    //- PatternTree
    case Bind(name, body, symbol)                               => body :: Nil
    case Unapply(fun, implicits, patterns)                      => fun :: implicits ::: patterns
    case Alternative(trees)                                     => trees
    case ExprPattern(expr)                                      => expr :: Nil
    case TypeTest(body, tpt)                                    => body :: tpt :: Nil
    case WildcardPattern(tpe)                                   => Nil

    //- TypeTree
    case AnnotatedTypeTree(tpt, annotation)                     => tpt :: annotation :: Nil
    case AppliedTypeTree(tycon, args)                           => tycon :: args
    case ByNameTypeTree(result)                                 => result :: Nil
    case MatchTypeTree(bound, selector, cases)                  => bound.toList ::: selector :: cases
    case RefinedTypeTree(underlying, refinements, refinedCls)   => underlying :: refinements
    case SelectTypeTree(qualifier, name)                        => qualifier :: Nil
    case SingletonTypeTree(ref)                                 => ref :: Nil
    case TermRefTypeTree(qualifier, name)                       => qualifier :: Nil
    case TypeIdent(name)                                        => Nil
    case TypeLambdaTree(tparams, body)                          => tparams ::: body :: Nil
    case TypeTreeBind(name, body, symbol)                       => body :: Nil
    case TypeWrapper(tp)                                        => Nil
    case WildcardTypeBoundsTree(bounds)                         => bounds :: Nil
    case TypeBindingsTree(bindings, body)                       => body :: bindings

    //- TypeDefinitionTree
    case TypeAliasDefinitionTree(alias)                         => alias :: Nil
    case OpaqueTypeAliasDefinitionTree(bounds, alias)           => bounds :: alias :: Nil
    case PolyTypeDefinitionTree(tparams, body)                  => tparams ::: body :: Nil
    case NamedTypeBoundsTree(name, bound)                       => Nil

    //-- TypeBoundsTree
    case InferredTypeBoundsTree(bounds)                         => Nil
    case ExplicitTypeBoundsTree(low, high)                      => low :: high :: Nil

  def reduce[T](op: Tree => T)(red: (T, List[T]) => T): T =
    def rec(t: Tree): T =
      val st = t.subtrees
      if st.isEmpty then op(t) else red(op(t), st.map(rec))
    rec(tree)


extension (tpe: Type)
  def subtypes: List[Type] = tpe match
    case tpe: PackageRef                => Nil
    case tpe: TermRef                   => tpe.prefix match
      case prefix: Type => prefix :: Nil
      case _            => Nil
    case tpe: TypeRef                   => tpe.prefix match
      case prefix: Type => prefix :: Nil
      case _            => Nil
    case tpe: MethodType                => tpe.resultType :: tpe.paramTypes
    case tpe: PolyType                  => tpe.resultType :: tpe.paramTypeBounds.flatMap(_.subtypes)  //tpe.resultType :: Nil
    case tpe: AppliedType               => tpe.tycon :: tpe.args
    case tpe: ByNameType                => tpe.resultType :: Nil
    case tpe: ThisType                  => tpe.tref :: Nil
    case tpe: OrType                    => tpe.first :: tpe.second :: Nil
    case tpe: AndType                   => tpe.first :: tpe.second :: Nil
    case tpe: TypeLambda                => tpe.resultType :: tpe.paramTypeBounds.flatMap(_.subtypes)  //tpe.resultType :: Nil
    case tpe: TypeParamRef              => tpe.binders.paramTypeBounds(tpe.paramNum).subtypes  //Nil
    case tpe: TermParamRef              => tpe.binders.paramTypes(tpe.paramNum) :: Nil  //Nil
    case tpe: AnnotatedType             => tpe.typ :: Nil
    case tpe: ConstantType              => Nil
    case tpe: MatchType                 => tpe.bound :: tpe.scrutinee :: tpe.cases.flatMap(c => c.pattern :: c.result :: c.paramTypeBounds.flatMap(_.subtypes))  //tpe.scrutinee :: tpe.cases.flatMap(c => c.pattern :: c.result :: c.paramTypeBounds.flatMap(_.subtypes))
    case tpe: RecType                   => tpe.parent :: Nil
    case tpe: RecThis                   => tpe.binders :: Nil
    case tpe: SuperType                 => tpe.thistpe :: tpe.explicitSupertpe.toList  //tpe.thistpe :: Nil
    case tpe: TypeRefinement            => tpe.parent :: tpe.refinedBounds.subtypes  //tpe.parent :: Nil
    case tpe: TermRefinement            => tpe.parent :: tpe.refinedType :: Nil
    case tpe: WildcardTypeBounds        => tpe.bounds.subtypes  //Nil
    //case tpe: SkolemType              => ???  //*
    //case _: CustomTransientGroundType => ???  //*

  def reduce[T](op: Type => T)(red: (T, List[T]) => T): T =
    def rec(t: Type): T =
      val st = t.subtypes
      if st.isEmpty then op(t) else red(op(t), st.map(rec))
    rec(tpe)

  def isRef(sym: Symbol)(using Context): Boolean = tpe match
    case tpe: NamedType     => tpe.optSymbol.contains(sym)
    case tpe: AppliedType   => tpe.underlying.isRef(sym)
    case tpe: TermParamRef  => tpe.underlying.isRef(sym)
    case tpe: TypeParamRef  => tpe.bounds.high.isRef(sym)
    case _                  => false

  def isOfClass(cls: ClassSymbol)(using Context): Boolean = tpe match
    case tpe: TermRef       => tpe.underlying.isOfClass(cls)
    case tpe: ConstantType  => tpe.underlying.isOfClass(cls)
    case _                  => tpe.isRef(cls)


extension (tb: TypeBounds)
  def subtypes: List[Type] = tb match
    case tb: RealTypeBounds => tb.low :: tb.high :: Nil
    case tb: TypeAlias      => tb.alias :: Nil
