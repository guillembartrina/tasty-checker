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

class ExprTypeConformanceTestSuite extends BaseTestSuite:

  object Data:
    given Context = TestData.test_auxiliar_context

    val defaultTermName: TermName = termName("termName")
    val defaultTermSymbol: TermSymbol = defn.Any_toString
    val defaultTypeTree: TypeTree = TypeIdent(typeName("Int"))(defn.IntType)(NoSpan)
    val defaultTermTree: TermTree = Literal(Constant(0))(NoSpan)
    val defaultPatternTree: PatternTree = WildcardPattern(defn.IntType)(NoSpan)

    private def getTerm(c: ClassSymbol)(n: String): TermSymbol = c.getMember(termName(n)).get
    private def getMeth(c: ClassSymbol)(n: String): TermSymbol = c.getNonOverloadedDecl(termName(n)).get
    private def decomposeValDef(t: TermSymbol): (Type, TypeTree, TermTree) =
      val tree = t.tree.get.asInstanceOf[ValDef]
      (tree.tpt.toType, tree.tpt, tree.rhs.get)

    val auxiliarPackage: PackageSymbol = defn.RootPackage.getPackageDecl(Names.termName("auxiliar")).get
    val constructsClass: ClassSymbol = auxiliarPackage.getDecl(Names.moduleClassName("Constructs")).get.asClass

    val getTermFromConstructs = getTerm(constructsClass)
    val getMethFromConstructs = getMeth(constructsClass)

    //val (anyKindType, anyKindTypeTree, _) = (defn.AnyKindType, TypeIdent(typeName("AnyKind"))(defn.AnyKindType)(NoSpan), null)
    val anyFull @ (anyType, anyTypeTree, anyTermTree) = decomposeValDef(getTermFromConstructs("any"))
    val anyValFull @ (anyValType, anyValTypeTree, anyValTermTree) = decomposeValDef(getTermFromConstructs("anyVal"))
    val booleanFull @ (booleanType, booleanTypeTree, booleanTermTree) = decomposeValDef(getTermFromConstructs("boolean"))
    val _falseFull @ (_falseType, _falseTypeTree, _falseTermTree) = decomposeValDef(getTermFromConstructs("_false"))
    val stringFull @ (stringType, stringTypeTree, stringTermTree) = decomposeValDef(getTermFromConstructs("string"))

    val methodBooleanToBooleanTermSymbol = getMethFromConstructs("methodBooleanToBoolean")
    val funBooleanToBooleanTermTree = decomposeValDef(getTermFromConstructs("funBooleanToBoolean"))._3.asInstanceOf[Apply].fun

    val anyRefFull @ (anyRefType, anyRefTypeTree, anyRefTermTree) = decomposeValDef(getTermFromConstructs("anyRef"))
    val throwableFull @ (throwableType, throwableTypeTree, throwableTermTree) = decomposeValDef(getTermFromConstructs("throwable"))
    val nullFull @ (nullType, nullTypeTree, nullTermTree) = decomposeValDef(getTermFromConstructs("_null"))

    val unitFull @ (unitType, unitTypeTree, unitTermTree) = decomposeValDef(getTermFromConstructs("unit"))

    val seqBooleanFull @ (seqBooleanType, seqBooleanTypeTree, seqBooleanTermTree) = decomposeValDef(getTermFromConstructs("seqBoolean"))
    val repeatedBooleanFull @ (repeatedType, repeatedTypeTree, repeatedTermTree) = (
      defn.RepeatedTypeOf(booleanType),
      AppliedTypeTree(TypeIdent(Names.typeName("<repeated>"))(defn.RepeatedTypeUnapplied)(NoSpan), List(booleanTypeTree))(NoSpan),
      Typed(SeqLiteral(List(Literal(Constant(0))(NoSpan)), booleanTypeTree)(NoSpan),
        AppliedTypeTree(TypeIdent(Names.typeName("<repeated>"))(defn.RepeatedTypeUnapplied)(NoSpan), List(booleanTypeTree))(NoSpan))(NoSpan)
    )

    val methodByNameBooleanToBooleanTermSymbol = getMethFromConstructs("methodByNameBooleanToBoolean")
    val funByNameBooleanToBooleanTermTree = decomposeValDef(getTermFromConstructs("funByNameBooleanToBoolean"))._3.asInstanceOf[Apply].fun

    /*
    val termSymbolFunStringToAnyRef: TermSymbol = constructsClass.getNonOverloadedDecl(Names.termName("funStringToAnyRef")).get

    val termSymbolFunStringToString: TermSymbol = constructsClass.getNonOverloadedDecl(Names.termName("funStringToString")).get
    val termSymbolFunByNameStringToString: TermSymbol = constructsClass.getNonOverloadedDecl(Names.termName("funByNameStringToString")).get

    val termSymbolFunStringToNull: TermSymbol = constructsClass.getNonOverloadedDecl(Names.termName("funStringToNull")).get
    val termSymbolFunStringToInt: TermSymbol = constructsClass.getNonOverloadedDecl(Names.termName("funStringToInt")).get

    val typeFunStringToAnyRef: Type = termSymbolFunStringToAnyRef.declaredType
    val typeFunStringToString: Type = termSymbolFunStringToString.declaredType
    val typeFunStringToNull: Type = termSymbolFunStringToNull.declaredType
    val typeFunStringToInt: Type = termSymbolFunStringToInt.declaredType

    val typeTreeFunStringToAnyRef: TypeTree = TypeIdent(Names.typeName("_"))(typeFunStringToAnyRef)(NoSpan)
    val typeTreeFunStringToString: TypeTree = TypeIdent(Names.typeName("_"))(typeFunStringToString)(NoSpan)
    val typeTreeFunStringToNull: TypeTree = TypeIdent(Names.typeName("_"))(typeFunStringToNull)(NoSpan)
    val typeTreeFunStringToInt: TypeTree = TypeIdent(Names.typeName("_"))(typeFunStringToInt)(NoSpan)

    val termTreeFunStringToAnyRef: TermTree =
      Ident(Names.termName("funStringToAnyRef"))
        (TermRef(ThisType(TypeRef(auxiliarPackage.packageRef, constructsClass)), termSymbolFunStringToAnyRef))(NoSpan)
    val termTreeFunStringToString: TermTree =
      Ident(Names.termName("funStringToString"))
        (TermRef(ThisType(TypeRef(auxiliarPackage.packageRef, constructsClass)), termSymbolFunStringToString))(NoSpan)
    val termTreeFunByNameStringToString: TermTree =
      Ident(Names.termName("funByNameStringToString"))
        (TermRef(ThisType(TypeRef(auxiliarPackage.packageRef, constructsClass)), termSymbolFunByNameStringToString))(NoSpan)
    val termTreeFunStringToNull: TermTree =
      Ident(Names.termName("funStringToNull"))
        (TermRef(ThisType(TypeRef(auxiliarPackage.packageRef, constructsClass)), termSymbolFunStringToNull))(NoSpan)
    val termTreeFunStringToInt: TermTree =
      Ident(Names.termName("funStringToInt"))
        (TermRef(ThisType(TypeRef(auxiliarPackage.packageRef, constructsClass)), termSymbolFunStringToInt))(NoSpan)

    val StringToStringType: TypeRef = TypeRef(constructsClass.appliedRef, typeName("StringToString"))
    val typeTreeStringtoString: TypeTree = TypeIdent(Names.typeName("StringToString"))(StringToStringType)(NoSpan)
    */


  import Data.*

  private val testWithTestAuxiliarContext = testWithContext(TestData.test_auxiliar_context)

  private def assertChecksWithNoProblems(tree: Tree)(using Context): Unit =
    val checker = Checker(Check.allChecks)
    checker.check(tree)
    assertNoProblems(checker.problems)

  private def testAssumptions
    (types: List[Type], typeTrees: List[TypeTree], termTrees: List[TermTree], decider: (Int, Int) => Boolean)
    (using Context): Unit =
        def helper[A, B](list1: List[A], list2: List[B], decider: (Int, Int) => Boolean)(builder: (A, B) => (Type, Type)): Unit =
          for
            (a, i) <- list1.zipWithIndex
            (b, j) <- list2.zipWithIndex
          do
            val (x, y) = builder(a, b)
            val res = if decider(i, j) then !x.isSubtype(y) else x.isSubtype(y)
            assert(res, clues(i, j, decider(i, j), x, y))

        helper(typeTrees, typeTrees, decider){ (a, b) => (a.toType, b.toType)}
        helper(typeTrees, types, decider){ (a, b) => (a.toType, b)}
        helper(typeTrees, termTrees, decider){ (a, b) => (a.toType, b.tpe)}

        helper(types, typeTrees, decider){ (a, b) => (a, b.toType)}
        helper(types, types, decider){ (a, b) => (a, b)}
        helper(types, termTrees, decider){ (a, b) => (a, b.tpe)}

        helper(termTrees, typeTrees, decider){ (a, b) => (a.tpe, b.toType)}
        helper(termTrees, types, decider){ (a, b) => (a.tpe, b)}
        helper(termTrees, termTrees, decider){ (a, b) => (a.tpe, b.tpe)}

  private def exhaustiveTestGenerator[A, B]
    (lista: List[A], listb: List[B], decider: ((A, Int), (B, Int)) => Boolean)
    (builder: (A, B) => (Span => Tree, TermTree, Type))
    (using Context): Unit =
      for
        (a, i) <- lista.zipWithIndex
        (b, j) <- listb.zipWithIndex
      yield
        val (t, term, tpe) = builder(a, b)
        val tree = t(NoSpan)
        val checker = Checker(Check.checks(List("ExprTypeConformance")))
        checker.check(tree)
        if decider((a, i), (b, j))
        then assertProblems(checker.problems, List(NotConformsType(term.tpe, tpe, tree)))
        else assertNoProblems(checker.problems)
      

  private def defaultExhaustiveTests(using Context) = exhaustiveTestGenerator(
    List(anyValTermTree, booleanTermTree, _falseTermTree, stringTermTree),
    List(booleanFull),
    { case ((_, i), (_, _)) => List(0, 3).contains(i) }
  )
  // This also works for boolean

  testWithTestAuxiliarContext("assumptions_default") {
    testAssumptions(
      List(anyValType, booleanType, _falseType, stringType),
      List(anyValTypeTree, booleanTypeTree, _falseTypeTree, stringTypeTree),
      List(anyValTermTree, booleanTermTree, _falseTermTree, stringTermTree),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )
  }

  //TODO: template

  testWithTestAuxiliarContext("tree_CaseDef") {
    defaultExhaustiveTests{ case (guard, _) =>
      (CaseDef(defaultPatternTree, Some(guard), defaultTermTree), guard, defn.BooleanType)
    }

    assertChecksWithNoProblems(CaseDef(defaultPatternTree, None, defaultTermTree)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_ValDef") {
    defaultExhaustiveTests{ case (rhs, (_, tpt, _)) =>
      (ValDef(defaultTermName, tpt, Some(rhs), defaultTermSymbol), rhs, tpt.toType)
    }

    assertChecksWithNoProblems(ValDef(defaultTermName, defaultTypeTree, None, defaultTermSymbol)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_DefDef") {
    defaultExhaustiveTests{ case (rhs, (_, resultTpt, _)) =>
      (DefDef(defaultTermName, Nil, resultTpt, Some(rhs), defaultTermSymbol), rhs, resultTpt.toType)
    }

    assertChecksWithNoProblems(DefDef(defaultTermName, Nil, defaultTypeTree, None, defaultTermSymbol)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_Apply") {
    defaultExhaustiveTests{ case (arg, _) =>
      (Apply(funBooleanToBooleanTermTree, List(arg)), arg,
      funBooleanToBooleanTermTree.tpe.widen.asInstanceOf[MethodType].paramTypes(0))
    }

    defaultExhaustiveTests{ case (arg, _) =>
      (Apply(funByNameBooleanToBooleanTermTree, List(arg)), arg,
      funByNameBooleanToBooleanTermTree.tpe.widen.asInstanceOf[MethodType].paramTypes(0).asInstanceOf[ByNameType].underlying)
    }

    assertChecksWithNoProblems(Apply(funBooleanToBooleanTermTree, Nil)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_Assign") {
    defaultExhaustiveTests{ case (rhs, (_, _, lhs)) =>
      (Assign(lhs, rhs), rhs, lhs.tpe.widen)
    }
  }

  //TODO: block

  testWithTestAuxiliarContext("tree_If") {
    defaultExhaustiveTests{ case (cond, _) =>
      (If(cond, defaultTermTree, defaultTermTree), cond, defn.BooleanType)
    }
  }

  testWithTestAuxiliarContext("tree_InlineIf") {
    defaultExhaustiveTests{ case (cond, _) =>
      (InlineIf(cond, defaultTermTree, defaultTermTree), cond, defn.BooleanType)
    }
  }

  //TODO: inlineMatch

  /*
  testWithTestAuxiliarContext("LSP-Lambda") {
    val lambdaExhaustiveTests = exhaustiveTestGenerator(
      List(typeTreeStringtoString), List(termTreeFunStringToAnyRef, termTreeFunStringToString, termTreeFunStringToNull, termTreeFunStringToInt),
      { case ((_, _), (_, j)) => List(0, 3).contains(j) }
    )

    /*
    testAssumptions(
      List(typeTreeFunStringToAnyRef, typeTreeFunStringToString, typeTreeFunStringToNull, typeTreeFunStringToInt),
      List(typeFunStringToAnyRef, typeFunStringToString, typeFunStringToNull, typeFunStringToInt),
      List(termTreeFunStringToAnyRef, termTreeFunStringToString, termTreeFunStringToNull, termTreeFunStringToInt),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )
    It fails because of the bug regarding MethodType and isSubtype (see PotentialBugsSuite)
    */

    /*
    lambdaExhaustiveTests{ case (sam, meth) =>
      (Lambda(meth, Some(sam)), meth.tpe,
      sam.toType.widen.asInstanceOf[TypeRef].optSymbol.get.asClass.declarations
      .filter(x => x.flags.is(Flags.Abstract) && x.name != nme.Constructor).map(_.asTerm).head.declaredType)
    }
    It fails because of the bug regarding MethodType and isSubtype (see PotentialBugsSuite)
    */

    assertChecksWithNoProblems(Lambda(termTree, None)(NoSpan))
  }
  */

  //TODO: match

  testWithTestAuxiliarContext("tree_Return") {
    defaultExhaustiveTests{ case (expr, _) =>
      (Return(Some(expr), methodBooleanToBooleanTermSymbol), expr,
      methodBooleanToBooleanTermSymbol.declaredType.asInstanceOf[MethodType].resultType)
    }

    assertChecksWithNoProblems(Return(None, methodBooleanToBooleanTermSymbol)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_SeqLiteral") {
    defaultExhaustiveTests{ case (elem, (_, elemtpt, _)) =>
      (SeqLiteral(List(elem), elemtpt), elem, elemtpt.toType)
    }

    assertChecksWithNoProblems(SeqLiteral(Nil, defaultTypeTree)(NoSpan))
  }

  testWithTestAuxiliarContext("LSP-Throw") {
    val throwExhaustiveTests = exhaustiveTestGenerator(
      List(anyRefTermTree, throwableTermTree, nullTermTree, booleanTermTree),
      List(null),
      { case ((_, i), (_, _)) => List(0, 3).contains(i) }
    )

    testAssumptions(
      List(anyRefType, throwableType, nullType, booleanType),
      List(anyRefTypeTree, throwableTypeTree, nullTypeTree, booleanTypeTree),
      List(anyRefTermTree, throwableTermTree, nullTermTree, booleanTermTree),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )

    throwExhaustiveTests{ case (expr, _) =>
      (Throw(expr), expr, defn.ThrowableType)
    }
  }

  private def unitExhaustiveTests(using Context) = exhaustiveTestGenerator(
    List(anyValTermTree, unitTermTree, stringTermTree),
    List(null),
    { case ((_, i), (_, _)) => List(0, 2).contains(i) }
  )

  testWithTestAuxiliarContext("assumptions_unit") {
    testAssumptions(
      List(anyValType, unitType, stringType),
      List(anyValTypeTree, unitTypeTree, stringTypeTree),
      List(anyValTermTree, unitTermTree, stringTermTree),
      (i: Int, j: Int) => (j > i) || (i == 2 && j != 2)
    )
  }

  testWithTestAuxiliarContext("tree_Try") {
    unitExhaustiveTests{ case (expr, _) =>
      (Try(defaultTermTree, List(), Some(expr)), expr, defn.UnitType)
    }

    assertChecksWithNoProblems(Try(defaultTermTree, List(), None)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_Typed") {
    defaultExhaustiveTests{ case (expr, (_, tpt, _)) =>
      (Typed(expr, tpt), expr, tpt.toType)
    }

    val repeatedExhaustiveTests = exhaustiveTestGenerator(
      List(anyRefTermTree, seqBooleanTermTree, nullTermTree, booleanTermTree),
      List(repeatedBooleanFull),
      { case ((_, i), (_, _)) => List(0, 3).contains(i) }
    )

    testAssumptions(
      List(anyRefType, seqBooleanType, nullType, booleanType),
      List(anyRefTypeTree, seqBooleanTypeTree, nullTypeTree, booleanTypeTree),
      List(anyRefTermTree, seqBooleanTermTree, nullTermTree, booleanTermTree),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )

    repeatedExhaustiveTests{ case (expr, (_, tpt, _)) =>
      (Typed(expr, tpt), expr,
      defn.SeqTypeOf(tpt.toType.asInstanceOf[AppliedType].args(0)))
    }
  }

  testWithTestAuxiliarContext("tree_While") {
    defaultExhaustiveTests{ case (cond, _) =>
      (While(cond, unitTermTree), cond, defn.BooleanType)
    }

    unitExhaustiveTests{ case (expr, _) =>
      (While(booleanTermTree, expr), expr, defn.UnitType)
    }
  }
