package tastychecker

import scala.collection.mutable as mut
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

// -------------------------------------------------------

class Checker(val checks: List[Check]):

  private val _problems: mut.ListBuffer[Problem] = mut.ListBuffer.empty[Problem]

  private def walker(tree: Tree)(using Context): Unit =
    checks.foreach(c => _problems.addAll(c.check(tree)))

  def check(tree: Tree)(using Context): Unit =
    tree.walkTree(walker)

  def check(trees: Iterable[Tree])(using Context): Unit =
    trees.foreach(check)

  def problems: List[Problem] = _problems.toList.reverse

// -------------------------------------------------------

/*
extension (tree: Tree)
  def subtrees: List[Tree] =
    tree match
      case CaseDef(pattern, guard, body)                          => pattern :: guard.toList ::: body :: Nil
      case ImportIdent(name)                                      => Nil
      case ImportSelector(imported, renamed, bound)               => imported :: renamed.toList ::: bound.toList
      case SelfDef(name, tpt)                                     => Nil //tpt?
      case Template(constr, parents, self, body)                  => constr :: parents ::: self.toList ::: body
      case TypeCaseDef(pattern, body)                             => pattern :: body :: Nil

      //- TopLevelTree
      case PackageDef(pid, stats)                                 => stats

      //-- StatementTree
      case Import(expr, selectors)                                => expr :: selectors
      case Export(expr, selectors)                                => expr :: selectors

      //--- ValOrDefDef
      case DefDef(name, paramLists, resultTpt, rhs, symbol)       => paramLists.flatMap((_.merge)) ::: resultTpt :: rhs.toList
      case ValDef(name, tpt, rhs, symbol)                         => tpt :: rhs.toList

      //--- TypeDef
      case ClassDef(name, rhs, symbol)                            => rhs :: Nil
      case TypeMember(name, rhs, symbol)                          => rhs :: Nil
      case TypeParam(name, bounds, symbol)                        => Nil //bounds?

      //--- TermTree
      case Apply(fun, args)                                       => fun :: args
      case Assign(lhs, rhs)                                       => lhs :: rhs :: Nil
      case Block(stats, expr)                                     => stats :+ expr
      case Ident(name)                                            => Nil
      case If(cond, thenPart, elsePart)                           => cond :: thenPart :: elsePart :: Nil
      case InlineIf(cond, thenPart, elsePart)                     => cond :: thenPart :: elsePart :: Nil
      case InlineMatch(selector, cases)                           => selector.toList ::: cases
      case Inlined(expr, caller, bindings)                        => expr :: bindings //caller?
      case Lambda(meth, tpt)                                      => meth :: tpt.toList
      case Literal(constant)                                      => Nil
      case Match(selector, cases)                                 => selector :: cases
      case NamedArg(name, arg)                                    => arg :: Nil
      case New(tpt)                                               => Nil //tpt?
      case Return(expr, from)                                     => expr.toList
      case Select(qualifier, name)                                => qualifier :: Nil
      case SeqLiteral(elems, elempt)                              => elems ::: elempt :: Nil
      case Super(qual, mix)                                       => qual :: Nil //mix?
      case This(qualifier)                                        => Nil //qualifier?
      case Throw(expr)                                            => expr :: Nil
      case Try(expr, cases, finalizer)                            => (expr :: cases) ::: finalizer.toList
      case TypeApply(fun, args)                                   => fun :: args
      case Typed(expr, tpt)                                       => expr :: tpt :: Nil
      case While(cond, body)                                      => cond :: body :: Nil
      
      //- PatternTree
      case Bind(name, body, symbol)                               => body :: Nil
      case Unapply(fun, implicits, patterns)                      => fun :: implicits ::: patterns
      case Alternative(trees)                                     => trees
      case ExprPattern(expr)                                      => expr :: Nil
      case TypeTest(body, tpt)                                    => body :: tpt :: Nil
      case WildcardPattern(tpe)                                   => Nil //tpe?

      //- TypeTree
      case AnnotatedTypeTree(tpt, annotation)                     => tpt :: annotation :: Nil
      case AppliedTypeTree(tycon, args)                           => tycon :: args
      case ByNameTypeTree(result)                                 => result :: Nil
      case MatchTypeTree(bound, selector, cases)                  => bound.toList ::: selector :: cases
      case RefinedTypeTree(underlying, refinements, refinedCls)   => underlying :: refinements
      case SelectTypeTree(qualifier, name)                        => qualifier :: Nil
      case SingletonTypeTree(ref)                                 => ref :: Nil
      case TermRefTypeTree(qualifier, name)                       =>  qualifier :: Nil
      case TypeIdent(name)                                        => Nil
      case TypeLambdaTree(tparams, body)                          => tparams ::: body :: Nil
      case TypeTreeBind(name, body, symbol)                       => body :: Nil
      case TypeWrapper(tp)                                        => Nil //tp
      case WildcardTypeBoundsTree(bounds)                         => bounds :: Nil

      //- TypeDefinitionTree
      case TypeAliasDefinitionTree(alias)                         => alias :: Nil
      case OpaqueTypeAliasDefinitionTree(bounds, alias)           => bounds :: alias :: Nil
      case PolyTypeDefinitionTree(tparams, body)                  => tparams ::: body :: Nil
      case NamedTypeBoundsTree(name, bound)                       => Nil //bound?

      //-- TypeBoundsTree
      case InferredTypeBoundsTree(bounds)                         => Nil //bounds?
      case ExplicitTypeBoundsTree(low, high)                      => low :: high :: Nil
*/
