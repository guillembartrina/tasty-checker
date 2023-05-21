package tastychecker

import scala.collection.mutable as mut

import tastyquery.*
import tastyquery.Contexts.*
import tastyquery.Exceptions.*
import tastyquery.Spans.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*

class TypeBoundsConformanceTestSuite extends BaseTestSuite:

  object Data:
    given Context = TestData.test_auxiliar_context

    val defaultTypeName: TypeName = typeName("typeName")
    val defaultTermTree: TermTree = Literal(Constant(0))(NoSpan)

    private def getTerm(c: ClassSymbol)(n: String): TermSymbol = c.getMember(termName(n)).get
    private def decomposeValDef(t: TermSymbol): (Type, TypeTree) =
      val tree = t.tree.get.asInstanceOf[ValDef]
      (tree.tpt.toType, tree.tpt)
    val auxiliarPackage: PackageSymbol = defn.RootPackage.getPackageDecl(Names.termName("auxiliar")).get
    val constructsClass: ClassSymbol = auxiliarPackage.getDecl(Names.moduleClassName("Constructs")).get.asClass
    val getTermFromConstructs = getTerm(constructsClass)

    val anyValFull @ (anyValType, anyValTypeTree) = decomposeValDef(getTermFromConstructs("anyVal"))
    val booleanFull @ (booleanType, booleanTypeTree) = decomposeValDef(getTermFromConstructs("boolean"))
    val _falseFull @ (_falseType, _falseTypeTree) = decomposeValDef(getTermFromConstructs("_false"))
    val stringFull @ (stringType, stringTypeTree) = decomposeValDef(getTermFromConstructs("string"))

  import Data.*

  private val testWithTestAuxiliarContext = testWithContext(TestData.test_auxiliar_context)

  private def assertChecksWithNoProblems(tree: Tree)(using Context): Unit =
    val checker = Checker(Check.allChecks)
    checker.check(tree)
    assertNoProblems(checker.problems)

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
    (builder: (A, B, B) => (Span => Tree, Type, TypeBounds))
    (using Context): Unit =
      for
        (a, i) <- lista.zipWithIndex
        (b, j) <- listb.zipWithIndex
        (c, k) <- listb.zipWithIndex
      yield
        val (t, tpe, tb) = builder(a, b, c)
        val tree = t(NoSpan)
        val checker = Checker(Check.checks(List("TypeBoundsConformance")))
        checker.check(tree)
        if decider((a, i), (b, j), (c, k))
        then assertProblems(checker.problems, List(NotConformsBounds(tpe, tb, tree)))
        else assertNoProblems(checker.problems)
      
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
      val fun = New(TypeWrapper(PolyType(List(defaultTypeName), List(RealTypeBounds(low, high)), defn.BooleanType))(NoSpan))(NoSpan)
      (TypeApply(fun, List(arg)), arg.toType, RealTypeBounds(low, high))
    }

    assertChecksWithNoProblems(TypeApply(New(TypeWrapper(PolyType(Nil, Nil, defn.BooleanType))(NoSpan))(NoSpan), Nil)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_AppliedTypeTree") {  //Very hacky test :/
    defaultExhaustiveTests{ case (arg, low, high) =>
      val fun = TypeWrapper(AppliedType(TypeLambda(List(defaultTypeName), List(RealTypeBounds(defn.BooleanType, defn.BooleanType)),
        TypeLambda(List(defaultTypeName), List(RealTypeBounds(low, high)), defn.BooleanType)), List(defn.BooleanType)))(NoSpan)
      (AppliedTypeTree(fun, List(arg)), arg.toType, RealTypeBounds(low, high))
    }

    assertChecksWithNoProblems(AppliedTypeTree(TypeWrapper(TypeLambda(Nil, Nil, defn.BooleanType))(NoSpan), Nil)(NoSpan))
  }

  