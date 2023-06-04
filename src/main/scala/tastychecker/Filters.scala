package tastychecker

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Flags.*
import tastyquery.Symbols.*
import tastyquery.Exceptions.*
import tastyquery.Trees.*
import tastyquery.Types.*


type TreeFilter = Filter[Tree]

object TreeFilter:
  def empty: TreeFilter = Filter.empty[Tree]

  //Filter of the current bugs/TODOs in tasty-query or dotty
  def default: TreeFilter =
    import FilterUtils.*
    Filter({
      case tree if exists(tree, Filter({
        case Select(qualifier, SignedName(SimpleName("apply"), _, _)) if qualifier.tpe.widen.isOfClass(ctx.findTopLevelClass("scala.PolyFunction")) =>  //PolyFunction
      })) =>
      case tree if exists(tree, Filter({
        case tpe: TypeTree if tpe.toType.isInstanceOf[TypeRef] && tpe.toType.asInstanceOf[TypeRef].optSymbol.get.asType.isOpaqueTypeAlias =>  //Opaque Type Aliases
      })) =>
      case tree if exists(tree, Filter({
        case tpe: TypeTree if tpe.toType.widen.isOfClass(ctx.findTopLevelClass("scala.Tuple2")) =>  //Tuple/Tuple2
      })) =>
      case tree if exists(tree, Filter({
        case term: TermTree if term.tpe.widen.isOfClass(ctx.findTopLevelClass("java.lang.Class")) =>  //Class
      })) =>
      case ValDef(_, _, _, symbol) if throws[UnsupportedOperationException](symbol.signature) =>  //Erasure: AndType
      case DefDef(_, _, _, _, symbol) if throws[UnsupportedOperationException](symbol.signature) =>  //Erasure: AndType
      
      //TEMPORARY
      case tree if exists(tree, Filter({
        case tpe: TypeTree if tpe.toType.isInstanceOf[AppliedType] && tpe.toType.asInstanceOf[AppliedType].tycon.isInstanceOf[TypeLambda] =>  //AppliedType
        case term: TermTree if term.tpe.widen.isInstanceOf[AppliedType] && term.tpe.widen.asInstanceOf[AppliedType].args(0).isInstanceOf[WildcardTypeBounds] =>  //AppliedType
        case wc: WildcardPattern if exists(wc.tpe, Filter({
          case tf: TypeRef if tf.prefix == NoPrefix =>
        })) =>  //Type bindings in matches
      })) =>
      case tree if portalTreeToType(tree).exists{
        exists(_, Filter({
          case tpe: AppliedType if tpe.args.exists{
            case wtp: WildcardTypeBounds => wtp.bounds.high.isOfClass(ctx.findTopLevelClass("scala.AnyKind"))
            case _ => false
          } =>
        })
        )
      } =>  //Nothing-AnyKind in Nothing-Any
    })

// ----------

private type Filter[T] = Context ?=> PartialFunction[T, Unit]

extension [T](filter: Filter[T])
  private def matches(t: T)(using Context): Boolean = filter.isDefinedAt(t)

private object Filter:
  def apply[T](filter: Context ?=> PartialFunction[T, Unit]): Filter[T] = filter
  def empty[T]: Filter[T] = PartialFunction.empty[T, Unit]


private object FilterUtils:
  def exists[T: Treelike](t: T, filter: Filter[T])(using Context): Boolean =
    def rec(t: T): Boolean = t.children.map(rec).foldLeft(false)((x, y) => y || x) || filter.matches(t)
    rec(t)
  def throws[E <: Exception](body: => Unit): Boolean =
    try { body; false } catch { case _: E => true }
