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

class LSPTestSuite extends BaseTestSuite:

  object Data:
    given Context = TestData.testlib_dummy_context

    val RuntimeExceptionType = TypeRef(defn.javaLangPackage.packageRef, typeName("RuntimeException"))

    val termName: TermName = Names.termName("termName")
    val termSymbol: TermSymbol = defn.Any_toString
    val typeTree: TypeTree = TypeIdent(Names.typeName("Int"))(defn.IntType)(NoSpan)
    val termTree: TermTree = Literal(Constant(0))(NoSpan)
    val patternTree: PatternTree = WildcardPattern(defn.IntType)(NoSpan)

    val typeAnyVal: Type = {val x = defn.AnyValType; x.optSymbol; x}
    val typeInt: Type = {val x = defn.IntType; x.optSymbol; x}
    val typeBoolean: Type = {val x = defn.BooleanType; x.optSymbol; x}
    val typeAnyRef: Type = {val x = defn.AnyRefType; x.optSymbol; x}
    val typeString: Type = {val x = defn.StringType; x.optSymbol; x}
    val typeSeqInt: Type = {val x = defn.SeqTypeOf(typeInt); x.translucentSuperType; x}
    val typeRepeatedInt: Type = {val x = defn.RepeatedTypeOf(typeInt); x.translucentSuperType; x}
    val typeThrowable: Type = {val x = defn.ThrowableType; x.optSymbol; x}
    val typeRuntimeException: Type = {val x = RuntimeExceptionType; x.optSymbol; x}
    val typeNull: Type = {val x = defn.NullType; x.optSymbol; x}

    val typeTreeAnyVal: TypeTree = TypeIdent(Names.typeName("AnyVal"))(typeAnyVal)(NoSpan)
    val typeTreeInt: TypeTree = TypeIdent(Names.typeName("Int"))(typeInt)(NoSpan)
    val typeTreeBoolean: TypeTree = TypeIdent(Names.typeName("Boolean"))(typeBoolean)(NoSpan)
    val typeTreeAnyRef: TypeTree = TypeIdent(Names.typeName("AnyRef"))(typeAnyRef)(NoSpan)
    val typeTreeString: TypeTree = TypeIdent(Names.typeName("String"))(typeString)(NoSpan)
    val typeTreeSeqInt: TypeTree = AppliedTypeTree(
      TypeIdent(Names.typeName("Seq"))(defn.SeqTypeUnapplied)(NoSpan),
      List(TypeIdent(Names.typeName("Int"))(defn.IntType)(NoSpan)))(NoSpan)
    val typeTreeRepeatedInt: TypeTree = AppliedTypeTree(
      TypeIdent(Names.typeName("<repeated>"))(defn.RepeatedTypeUnapplied)(NoSpan),
      List(TypeIdent(Names.typeName("Int"))(defn.IntType)(NoSpan)))(NoSpan)
    val typeTreeThrowable: TypeTree = TypeIdent(Names.typeName("Throwable"))(typeThrowable)(NoSpan)
    val typeTreeRuntimeException: TypeTree = TypeIdent(Names.typeName("RuntimeException"))(RuntimeExceptionType)(NoSpan)
    val typeTreeNull: TypeTree = TypeIdent(Names.typeName("Null"))(typeNull)(NoSpan)

    val termTreeAnyVal: TermTree = Typed(Literal(Constant(0))(NoSpan), typeTreeAnyVal)(NoSpan)
    val termTreeInt: TermTree = Literal(Constant(0))(NoSpan)
    val termTreeBoolean: TermTree = Literal(Constant(false))(NoSpan)
    val termTreeAnyRef: TermTree = Typed(Literal(Constant(null))(NoSpan), typeTreeAnyRef)(NoSpan)
    val termTreeString: TermTree = Literal(Constant("string"))(NoSpan)
    val termTreeSeqInt: TermTree = SeqLiteral(List(Literal(Constant(0))(NoSpan)), typeTreeInt)(NoSpan)
    val termTreeRepeatedInt: TermTree = Typed(SeqLiteral(List(Literal(Constant(0))(NoSpan)), typeTreeInt)(NoSpan), typeTreeRepeatedInt)(NoSpan)
    val termTreeThrowable: TermTree = Typed(Literal(Constant(null))(NoSpan), typeTreeThrowable)(NoSpan)
    val termTreeRuntimeException: TermTree = Typed(Literal(Constant(null))(NoSpan), typeTreeRuntimeException)(NoSpan)
    val termTreeNull: TermTree = Literal(Constant(null))(NoSpan)

    val termSymbolToString: TermSymbol = defn.Any_toString

    // ---

    val dummyPackage: PackageSymbol = defn.RootPackage.getPackageDecl(Names.termName("dummy")).get
    val auxiliarClass: ClassSymbol = dummyPackage.getDecl(Names.moduleClassName("Auxiliar")).get.asClass
    
    val termSymbolFunStringToAnyRef: TermSymbol = auxiliarClass.getNonOverloadedDecl(Names.termName("funStringToAnyRef")).get
    val termSymbolFunStringToString: TermSymbol = auxiliarClass.getNonOverloadedDecl(Names.termName("funStringToString")).get
    val termSymbolFunByNameStringToString: TermSymbol = auxiliarClass.getNonOverloadedDecl(Names.termName("funByNameStringToString")).get
    val termSymbolFunStringToNull: TermSymbol = auxiliarClass.getNonOverloadedDecl(Names.termName("funStringToNull")).get
    val termSymbolFunStringToInt: TermSymbol = auxiliarClass.getNonOverloadedDecl(Names.termName("funStringToInt")).get

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
        (TermRef(ThisType(TypeRef(dummyPackage.packageRef, auxiliarClass)), termSymbolFunStringToAnyRef))(NoSpan)
    val termTreeFunStringToString: TermTree =
      Ident(Names.termName("funStringToString"))
        (TermRef(ThisType(TypeRef(dummyPackage.packageRef, auxiliarClass)), termSymbolFunStringToString))(NoSpan)
    val termTreeFunByNameStringToString: TermTree =
      Ident(Names.termName("funByNameStringToString"))
        (TermRef(ThisType(TypeRef(dummyPackage.packageRef, auxiliarClass)), termSymbolFunByNameStringToString))(NoSpan)
    val termTreeFunStringToNull: TermTree =
      Ident(Names.termName("funStringToNull"))
        (TermRef(ThisType(TypeRef(dummyPackage.packageRef, auxiliarClass)), termSymbolFunStringToNull))(NoSpan)
    val termTreeFunStringToInt: TermTree =
      Ident(Names.termName("funStringToInt"))
        (TermRef(ThisType(TypeRef(dummyPackage.packageRef, auxiliarClass)), termSymbolFunStringToInt))(NoSpan)

    val StringToStringType: TypeRef = TypeRef(auxiliarClass.appliedRef, typeName("StringToString"))
    val typeTreeStringtoString: TypeTree = TypeIdent(Names.typeName("StringToString"))(StringToStringType)(NoSpan)


  import Data.*

  private val testWithBaseContext = testWithContext(TestData.testlib_dummy_context)

  private def assertChecksWithNoProblems(tree: Tree)(using Context): Unit =
    val checker = Checker(Check.allChecks)
    checker.check(tree)
    assertNoProblems(checker.problems)

  private def testAssumptions
    (typeTrees: List[TypeTree], types: List[Type], termTrees: List[TermTree], decider: (Int, Int) => Boolean)
    (using Context): Unit =
        def helper[A, B](list1: List[A], list2: List[B], decider: (Int, Int) => Boolean)(builder: (A, B) => (Type, Type)): Unit =
          for
            (a, i) <- list1.zipWithIndex
            (b, j) <- list2.zipWithIndex
          do
            val (x, y) = builder(a, b)
            val res = if decider(i, j) then !x.isSubtype(y) else x.isSubtype(y)
            assert(res, clues(x, y))

        helper(typeTrees, typeTrees, decider){ (a, b) => (a.toType, b.toType)}
        helper(typeTrees, types, decider){ (a, b) => (a.toType, b)}
        helper(typeTrees, termTrees, decider){ (a, b) => (a.toType, b.tpe.widen)}

        helper(types, typeTrees, decider){ (a, b) => (a, b.toType)}
        helper(types, types, decider){ (a, b) => (a, b)}
        helper(types, termTrees, decider){ (a, b) => (a, b.tpe.widen)}

        helper(termTrees, typeTrees, decider){ (a, b) => (a.tpe, b.toType)}
        helper(termTrees, types, decider){ (a, b) => (a.tpe, b)}
        helper(termTrees, termTrees, decider){ (a, b) => (a.tpe, b.tpe.widen)}

  private def exhaustiveTestGenerator[A, B]
    (lista: List[A], listb: List[B], decider: ((A, Int), (B, Int)) => Boolean)
    (builder: (A, B) => (Span => Tree, Type, Type))
    (using Context): Unit =
      for
        (a, i) <- lista.zipWithIndex
        (b, j) <- listb.zipWithIndex
      yield
        val (t, ta, tb) = builder(a, b)
        val tree = t(NoSpan)
        val checker = Checker(Check.checks(List("LSP")))
        checker.check(tree)
        if decider((a, i), (b, j))
        then assertProblems(checker.problems, List(NotSubtype(ta, tb, tree)))
        else assertNoProblems(checker.problems)
      

  private def defaultExhaustiveTests(using Context) = exhaustiveTestGenerator(
    List((typeTreeString, termTreeString)),
    List(termTreeAnyRef, termTreeString, termTreeNull, termTreeInt),
    { case ((_, _), (_, j)) => List(0, 3).contains(j) }
  )

  testWithBaseContext("LSPAssumptions-Default") {
    testAssumptions(
      List(typeTreeAnyRef, typeTreeString, typeTreeNull, typeTreeInt),
      List(typeAnyRef, typeString, typeNull, typeInt),
      List(termTreeAnyRef, termTreeString, termTreeNull, termTreeInt),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )
  }

  private def booleanExhaustiveTests(using Context) = exhaustiveTestGenerator(
    List(typeBoolean), List(termTreeAnyVal, termTreeBoolean, termTreeString),
    { case ((_, _), (_, j)) => List(0, 2).contains(j) }
  )

  testWithBaseContext("LSPAssumptions-Boolean") {
    testAssumptions(
      List(typeTreeAnyVal, typeTreeBoolean, typeTreeString),
      List(typeAnyVal, typeBoolean, typeString),
      List(termTreeAnyVal, termTreeBoolean, termTreeString),
      (i: Int, j: Int) => (j > i) || (i == 2 && j != 2)
    )
  }

  testWithBaseContext("LSP-CaseDef") {
    booleanExhaustiveTests{ case (boolean, guard) =>
      (CaseDef(patternTree, Some(guard), termTreeInt), guard.tpe, boolean)
    }

    assertChecksWithNoProblems(CaseDef(patternTree, None, termTreeInt)(NoSpan))
  }

  testWithBaseContext("LSP-ValDef") {
    defaultExhaustiveTests{ case ((tpt, _), term) =>
      (ValDef(termName, tpt, Some(term), termSymbol), term.tpe, tpt.toType)
    }

    assertChecksWithNoProblems(ValDef(termName, typeTree, None, termSymbol)(NoSpan))
  }

  testWithBaseContext("LSP-DefDef") {
    defaultExhaustiveTests{ case ((resultTpt, _), term) =>
      (DefDef(termName, Nil, resultTpt, Some(term), termSymbol), term.tpe, resultTpt.toType)
    }

    assertChecksWithNoProblems(DefDef(termName, Nil, typeTree, None, termSymbol)(NoSpan))
  }

  testWithBaseContext("LSP-Apply") {
    defaultExhaustiveTests{ case (_, arg) =>
      (Apply(termTreeFunStringToString, List(arg)), arg.tpe,
      termTreeFunStringToString.tpe.widen.asInstanceOf[MethodType].paramTypes(0))
    }

    defaultExhaustiveTests{ case (_, arg) =>
      (Apply(termTreeFunByNameStringToString, List(arg)), arg.tpe,
      termTreeFunByNameStringToString.tpe.widen.asInstanceOf[MethodType].paramTypes(0).asInstanceOf[ByNameType].underlying)
    }

    assertChecksWithNoProblems(Apply(termTreeFunStringToString, Nil)(NoSpan))

  }

  testWithBaseContext("LSP-Assign") {
    defaultExhaustiveTests{ case ((_, lhs), rhs) =>
      (Assign(lhs, rhs), rhs.tpe, lhs.tpe.widen)
    }
  }

  testWithBaseContext("LSP-If") {
    booleanExhaustiveTests{ case (boolean, cond) =>
      (If(cond, termTree, termTree), cond.tpe, boolean)
    }
  }

  testWithBaseContext("LSP-InlineIf") {
    booleanExhaustiveTests{ case (boolean, cond) =>
      (InlineIf(cond, termTree, termTree), cond.tpe, boolean)
    }
  }

  testWithBaseContext("LSP-Lambda") {
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

  testWithBaseContext("LSP-Return") {
    defaultExhaustiveTests{ case (_, term) =>
      (Return(Some(term), termSymbolToString), term.tpe,
      termSymbolToString.declaredType.asInstanceOf[MethodType].resultType)
    }

    assertChecksWithNoProblems(Return(None, termSymbolToString)(NoSpan))
  }

  testWithBaseContext("LSP-SeqLiteral") {
    defaultExhaustiveTests{ case ((elemtpt, _), elem) =>
      (SeqLiteral(List(elem), elemtpt), elem.tpe, elemtpt.toType)
    }

    assertChecksWithNoProblems(SeqLiteral(Nil, typeTree)(NoSpan))
  }

  testWithBaseContext("LSP-Super") {
    defaultExhaustiveTests{ case ((mix, _), qual) =>
      (Super(qual, Some(mix.asInstanceOf[TypeIdent])), qual.tpe, mix.toType)
    }
  }

  testWithBaseContext("LSP-Throw") {
    val throwableExhaustiveTests = exhaustiveTestGenerator(
      List(typeThrowable), List(termTreeAnyRef, termTreeThrowable, termTreeRuntimeException, termTreeInt),
      { case ((_, _), (_, j)) => List(0, 3).contains(j) }
    )

    testAssumptions(
      List(typeTreeAnyRef, typeTreeThrowable, typeTreeRuntimeException, typeTreeInt),
      List(typeAnyRef, typeThrowable, typeRuntimeException, typeInt),
      List(termTreeAnyRef, termTreeThrowable, termTreeRuntimeException, termTreeInt),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )

    throwableExhaustiveTests{ case (throwable, expr) =>
      (Throw(expr), expr.tpe, throwable)
    }
  }

  testWithBaseContext("LSP-Typed") {
    defaultExhaustiveTests{ case ((tpt, _), expr) =>
      (Typed(expr, tpt), expr.tpe, tpt.toType)
    }

    val typedExhaustiveTests = exhaustiveTestGenerator(
      List(typeTreeRepeatedInt), List(termTreeAnyRef, termTreeSeqInt, termTreeNull, termTreeInt),
      { case ((_, _), (_, j)) => List(0, 3).contains(j) }
    )

    testAssumptions(
      List(typeTreeAnyRef, typeTreeSeqInt, typeTreeNull, typeTreeInt),
      List(typeAnyRef, typeSeqInt, typeNull, typeInt),
      List(termTreeAnyRef, termTreeSeqInt, termTreeNull, termTreeInt),
      (i: Int, j: Int) => (j > i) || (i == 3 && j != 3)
    )

    typedExhaustiveTests{ case (tpt, expr) =>
      (Typed(expr, tpt), expr.tpe,
      defn.SeqTypeOf(tpt.toType.asInstanceOf[AppliedType].args(0)))
    }
  }

  testWithBaseContext("LSP-While") {
    booleanExhaustiveTests{ case (boolean, cond) =>
      (While(cond, termTree), cond.tpe, boolean)
    }
  }
