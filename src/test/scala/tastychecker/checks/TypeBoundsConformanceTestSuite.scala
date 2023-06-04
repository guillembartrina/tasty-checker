package tastychecker

import tastyquery.*
import tastyquery.Contexts.*
import tastyquery.Exceptions.*
import tastyquery.Spans.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*


class TypeBoundsConformanceTestSuite extends BaseCheckTestSuite:

  private object Data extends BaseData:
    val anyValFull @ (anyValType, anyValTypeTree, _) = decomposeValDef(getTerm("anyVal"))
    val booleanFull @ (booleanType, booleanTypeTree, _) = decomposeValDef(getTerm("boolean"))
    val _falseFull @ (_falseType, _falseTypeTree, _) = decomposeValDef(getTerm("_false"))
    val stringFull @ (stringType, stringTypeTree, _) = decomposeValDef(getTerm("string"))

  import Data.*

  private def assertChecksTypeBoundsConformance(using Context) = assertChecks(List(TypeBoundsConformance))

  private def testAssumptions
    (types: List[Type], typeTrees: List[TypeTree], decider: (Int, Int, Int) => Boolean)
    (using Context): Unit =
      def helper[A, B](list1: List[A], list2: List[B], decider: (Int, Int, Int) => Boolean)(builder: (A, B, B) => (Type, TypeBounds)): Unit =
        for
          (a, i) <- list1.zipWithIndex
          (b, j) <- list2.zipWithIndex
          (c, k) <- list2.zipWithIndex
        do
          val (x, y) = builder(a, b, c)
          val res = if decider(i, j, k) then !y.contains(x) else y.contains(x)
          assert(res, clues(i, j, k, decider(i, j, k), x, y))

      helper(types, types, decider){ (a, b, c) => (a, RealTypeBounds(b, c))}
      helper(types, typeTrees, decider){ (a, b, c) => (a, RealTypeBounds(b.toType, c.toType))}
      helper(typeTrees, types, decider){ (a, b, c) => (a.toType, RealTypeBounds(b, c))}
      helper(typeTrees, typeTrees, decider){ (a, b, c) => (a.toType, RealTypeBounds(b.toType, c.toType))}

  private def exhaustiveTestGenerator[A, B]
    (lista: List[A], listb: List[B], decider: ((A, Int), (B, Int), (B, Int)) => Boolean)
    (builder: (A, B, B) => (Span => Tree, Type, TypeBounds, Tree | Type))
    (using Context): Unit =
      for
        (a, i) <- lista.zipWithIndex
        (b, j) <- listb.zipWithIndex
        (c, k) <- listb.zipWithIndex
      yield
        val (t, tpe, tb, obj) = builder(a, b, c)
        val tree = t(NoSpan)
        val checker = TreeChecker(List(TypeBoundsConformance))
        if decider((a, i), (b, j), (c, k))
        then assertProblems(checker.check(tree), List(NotConformsBounds(tpe, tb, obj)))
        else assertNoProblems(checker.check(tree))
      
  private def defaultExhaustiveTests(using Context) = exhaustiveTestGenerator(
    List(anyValTypeTree, booleanTypeTree, _falseTypeTree, stringTypeTree),
    List(anyValType, booleanType, _falseType, stringType),
    { case ((_, i), (_, j), (_, k)) => val x = List(i, j, k).count(_ == 3); (x > 0 && x < 3) || j < i || k > i }
  )

  testWithTestAuxiliarContext("assumptions_default") {
    testAssumptions(
      List(anyValType, booleanType, _falseType, stringType),
      List(anyValTypeTree, booleanTypeTree, _falseTypeTree, stringTypeTree),
      (i: Int, j: Int, k: Int) => { val x = List(i, j, k).count(_ == 3); (x > 0 && x < 3) || j < i || k > i }
    )
  }

  testWithTestAuxiliarContext("tree_TypeApply") {  //Very hacky test :/
    defaultExhaustiveTests{ case (arg, low, high) =>
      val fun = termTreeFrom(PolyType(List(defaultTypeName), List(RealTypeBounds(low, high)), defn.BooleanType))
      (TypeApply(fun, List(arg)), arg.toType, RealTypeBounds(low, high), TypeApply(fun, List(arg))(NoSpan))
    }

    assertChecksTypeBoundsConformance(TypeApply(New(TypeWrapper(PolyType(Nil, Nil, defn.BooleanType))(NoSpan))(NoSpan), Nil)(NoSpan))
  }

  testWithTestAuxiliarContext("type_AppliedType") {  //Very hacky test :/
    defaultExhaustiveTests{ case (arg, low, high) =>
      val tycon = TypeLambda(List(defaultTypeName), List(RealTypeBounds(low, high)), defn.BooleanType)
      (TypeWrapper(AppliedType(tycon, List(arg.toType))), arg.toType, RealTypeBounds(low, high), AppliedType(tycon, List(arg.toType)))
    }

    assertChecksTypeBoundsConformance(TypeWrapper(TypeLambda(Nil, Nil, defn.BooleanType))(NoSpan))
  }
  