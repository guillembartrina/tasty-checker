package tastychecker

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Flags.*
import tastyquery.Symbols.*
import tastyquery.Exceptions.*
import tastyquery.Trees.*
import tastyquery.Types.*

type Filter = Context ?=> PartialFunction[Tree, Unit]

object Filter:
  def apply(filter: Context ?=> PartialFunction[Tree, Unit]): Filter = filter
  val empty: Filter = PartialFunction.empty[Tree, Unit]

extension (filter: Filter)
  def matches(tree: Tree)(using Context): Boolean = filter.isDefinedAt(tree)


object FilterUtils:
  def someSubtree(tree: Tree, filter: Filter)(using Context): Boolean =
    tree.walkTree(filter.matches)((x, y) => y|| x, false)
  def throwsIPSE[E <: InvalidProgramStructureException](body: => Unit): Boolean =
    try { body; false } catch { case _: E => true }
  
// -----

def currentFilter: Filter = Filter({
  case tree if FilterUtils.someSubtree(tree, Filter({
    case Select(qualifier, SignedName(SimpleName("apply"), _, _)) if qualifier.tpe.widen.isOfClass(ctx.findTopLevelClass("scala.PolyFunction")) =>  //PolyFunction
  })) =>
  case tree if FilterUtils.someSubtree(tree, Filter({
    case tpe: TypeTree if tpe.toType.isInstanceOf[TypeRef] && tpe.toType.asInstanceOf[TypeRef].optSymbol.get.asType.isOpaqueTypeAlias =>  //Opaque Type Aliases
  })) =>
  case tree if FilterUtils.someSubtree(tree, Filter({
    case tpe: TypeTree if tpe.toType.widen.isOfClass(ctx.findTopLevelClass("scala.Tuple2")) => //Tuple/Tuple2
  })) =>
  case tree if FilterUtils.someSubtree(tree, Filter({
    case term: TermTree if term.tpe.widen.isOfClass(ctx.findTopLevelClass("java.lang.Class")) =>  //Class
  })) =>

  case tree if FilterUtils.someSubtree(tree, Filter({
    case tpe: TypeTree if tpe.toType.isInstanceOf[AppliedType] && tpe.toType.asInstanceOf[AppliedType].tycon.isInstanceOf[TypeLambda] =>  //AppliedType
    case term: TermTree if term.tpe.widen.isInstanceOf[AppliedType] && term.tpe.widen.asInstanceOf[AppliedType].args(0).isInstanceOf[WildcardTypeBounds] =>  //TODO
  })) =>  //TEMPORARY
})
