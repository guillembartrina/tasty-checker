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

class PotentialBugsSuite extends BaseTestSuite:

  private val testSymbolWithTestlibTastyQueryContext = testSymbolWithContext(TestData.testlib_tastyquery_context)

  //private type TreePattern = PartialFunction[Tree, Unit]
  //private def findTree(tp: TreePattern)(t: Tree)(body: )

  // Develop specific checks
  private def assertChecksAndNoProblems(t: Tree)(using Context): Unit =
    val checker = Checker(Check.allChecks)
    checker.check(t)
    assertNoProblems(checker.problems)

  testSymbolWithTestlibTastyQueryContext("pb001_lambda-tpe-miss-impl")("simple_trees.ForExpressions/T") { symbol =>
    //println(symbol.tree.get)
    val problems = Checker(Check.allChecks).check(symbol.tree.get)
  } //TASTY_QUERY TODO -> IMPLEMENTED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb002_uninit-annot")("simple_trees.SpecialFunctionTypes/T") { symbol =>
    //println(symbol.tree.get)
    val problems = Checker(Check.allChecks).check(symbol.tree.get)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb003_sig-toplevel-init")("<empty>.toplevelEmptyPackage$package") { symbol =>
    val tree = symbol.tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select]

    val sig1 = tree.qualifier.tpe.asInstanceOf[TypeRef].optSymbol.get.asClass.findNonOverloadedDecl(nme.Constructor).signedName
    val sig2 = tree.name
    
    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
    assertEquals(sig1, sig2)
    //assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb004_sig-anon-class-constr")("simple_trees.ScalaEnum[$]") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.findNonOverloadedDecl(termName("$new")).tree.get.asInstanceOf[DefDef].rhs.get.asInstanceOf[Block]
      .expr.asInstanceOf[Typed].expr.asInstanceOf[Apply].fun.asInstanceOf[Select]
    )

    val sig1 = tree.tpe.asInstanceOf[TermRef].prefix.asInstanceOf[TypeRef].optSymbol.get.asClass.findNonOverloadedDecl(nme.Constructor).signedName
    val sig2 = tree.tpe.asInstanceOf[TermRef].name

    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
    assertEquals(sig1, sig2)
    //assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb005_sig-match")("simple_trees.MatchType/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("v")).get.tree.get.asInstanceOf[ValDef]
      .rhs.get.asInstanceOf[Apply].fun.asInstanceOf[TypeApply]
      .fun.asInstanceOf[Select]
    )

    val sig1 = tree.qualifier.tpe.asInstanceOf[ThisType].tref.optSymbol.get.asClass.findNonOverloadedDecl(termName("castMatchResult")).signedName
    val sig2 = tree.name

    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
    assertEquals(sig1, sig2)
    //assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.0

  testSymbolWithTestlibTastyQueryContext("pb006_subtyping-seq-and-repeated")("simple_trees.TypeApply/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("x")).get.tree.get.asInstanceOf[ValDef]
      .rhs.get.asInstanceOf[Apply]
      .args(0).asInstanceOf[Typed]
    )

    val tpe1 = tree.expr.tpe
    val tpe2 = tree.tpt.toType

    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(
      if tpe2.isOfClass(defn.RepeatedParamClass)
      then tpe1.isSubtype(defn.SeqTypeOf(tpe2.asInstanceOf[AppliedType].args(0)))
      else tpe1.isSubtype(tpe2)
    )
    //assertChecksAndNoProblems(tree)
  } //TASTY_CHECKER BUG -> FIXED

  testSymbolWithTestlibTastyQueryContext("pb007_wrong-resolution-bitset")("simple_trees.Repeated/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("f")).get.tree.get.asInstanceOf[DefDef]
    )
    
    val tree1 = tree.rhs.get.asInstanceOf[Apply].fun
    val tpe1 = tree.rhs.get.tpe
    val tpe2 = tree.resultTpt.toType

    //println(tree)
    //println("TREE A: " + )
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    //assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb008_wrong-resolution-function1")("simple_trees.QualThisType/T.Inner/T") { symbol =>  
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("withOp")).get.tree.get.asInstanceOf[DefDef]
    )

    val tree1 = tree.rhs.get.asInstanceOf[Apply].fun
    val tpe1 = tree.rhs.get.tpe
    val tpe2 = tree.resultTpt.toType.widen

    //println(tree)
    //println("TREE A: " + )
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    assertChecksAndNoProblems(tree)

    val tree2 = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("withOp")).get.tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply]
    )

    val tree21 = tree2.args(0)
    val tpe21 = tree2.args(0).tpe
    val tpe22 = tree2.fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0)

    //println(tree2)
    //println("TREE A: " + )
    //println("TYPE A: " + tpe21)
    //println("TYPE B: " + tpe22)
    assert(tpe21.isSubtype(tpe22))
    //assertChecksAndNoProblems(tree2)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb009_byname-wrong-subtyping")("simple_trees.OverloadedApply/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("callE")).get.tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply]
    )

    val tpe1 = tree.args(0).tpe
    val tpe2 = tree.fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0).asInstanceOf[ByNameType].underlying

    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    //assertChecksAndNoProblems(tree)
  } //TASTY_CHECKER BUG -> FIXED

  testSymbolWithTestlibTastyQueryContext("pb010_byname-wrong-subtyping2")("simple_trees.RefinedTypeTree/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("a")).get.tree.get.asInstanceOf[ValDef]
    )

    val tpe1 = tree.rhs.get.tpe
    val tpe2 = tree.tpt.toType.widen

    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    // --> assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.0

  testSymbolWithTestlibTastyQueryContext("pb011_typeparams-weird")("simple_trees.TypeRefIn/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("withArrayOfSubtypeAnyRef")).get.tree.get.asInstanceOf[DefDef] //1
      .rhs.get.asInstanceOf[Apply]
    )

    val tpe1 = tree.args(0).tpe
    val tpe2 = tree.fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0)

    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    //assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.3

  /*
  testSymbolWithTestlibTastyQueryContext("pb012_type-aliases")("simple_trees.AnyMethods/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("testGetClassInt")).get.tree.get.asInstanceOf[DefDef] //10
    )

    val tpe1 = tree.rhs.get.tpe
    val tpe2 = tree.resultTpt.toType

    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(!tpe1.isSubtype(tpe2))
    assert(!tpe2.isSubtype(tpe1))
    assert(!tpe1.isSameType(tpe2))
    assertChecksAndNoProblems(tree)
  } //COMPILER BUG
  */

  // ----------------

  testSymbolWithTestlibTastyQueryContext("pb013_sig-tlota")("crosspackagetasty.TopLevelOpaqueTypeAlias$package[$]") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol
    )

    val sig1 = tree.declarations(0).asDeclaringSymbol.declarations(2).asTerm.signedName
    val sig2 = tree.declarations(1).tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select].name
    
    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
    assertEquals(sig1, sig2)
    // TEMPORTAL: assertChecksAndNoProblems(tree.tree.get)
  } //TASTY_QUERY BUG -> FIXED 0.7.2

  testSymbolWithTestlibTastyQueryContext("pb014_weird-type")("simple_trees.MatchType/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("castMatchResultWithBind")).get.tree //9
    )

    val checker = Checker(Check.checks(List("ExprTypeConformance")))
    checker.check(tree)
    
    //checker.problems.foreach(x => { println(x); println() })
    assertEquals(checker.problems, List.empty[Problem])
  } //TASTY_QUERY BUG -> FIXED 0.7.2

  testSymbolWithTestlibTastyQueryContext("pb015_refined-types")("simple_trees.RefinedTypeTree/T") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("foo")).get.tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select]
    )

    val sig1 = tree.qualifier.tpe.widen.asInstanceOf[TypeRef].optSymbol.get.asDeclaringSymbol.declarations(1).asTerm.signedName
    val sig2 = tree.name

    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
    //assertEquals(sig1, sig2)
    assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> FIXED 0.7.2

  testSymbolWithTestlibTastyQueryContext("pb016_applied-types")("simple_trees.ForExpressions/T") { symbol =>
    val tree = (
      symbol.tree.get
    )
    val checker = Checker(Check.checks(List("ExprTypeConformance")))
    checker.check(tree)

    //checker.problems.foreach(x => { println(x); println() })
    assertEquals(checker.problems, List.empty[Problem])
  } //TASTY_QUERY BUG -> FIXED 0.7.2

  testSymbolWithTestlibTastyQueryContext("pb017_functions")("simple_trees.Function/T") { symbol =>
    val tree = (
      symbol.tree.get
    )
    val checker = Checker(Check.checks(List("ExprTypeConformance")))
    checker.check(tree)

    //checker.problems.foreach(x => { println("T " + x.asInstanceOf[NotSubtype].a.widen + "\n" + x.asInstanceOf[NotSubtype].b); println() })
    assertEquals(checker.problems, List.empty[Problem])
  } //TASTY_QUERY BUG -> FIXED 0.7.2

  testSymbolWithTestlibTastyQueryContext("part-functions")("simple_trees.WithPartialFunction/T") { symbol =>

    val checker = Checker(Check.checks(List("ExprTypeConformance")))
    checker.check(symbol.tree)
    
    //println(checker.problems(0).tree.asInstanceOf[Lambda].meth.tpe.widen)

    checker.problems.foreach(x => { println(x); println() })
  } //TASTY_CHECKER BUG

  // ----------------

  /* // The current API doen't allow to check opaque types because it works within the public universe (i.e we can check the subTyping relation between any two types) and doesn't use scopes
  testSymbolWithTestlibTastyQueryContext("pb018_subtyping-alias")("crosspackagetasty.TopLevelOpaqueTypeAlias$package[$]") { symbol =>
    val tree = (
      symbol.asClass.declarations(0).asClass.getNonOverloadedDecl(termName("apply")).get.tree.get.asInstanceOf[DefDef]
    )

    val tpe1 = tree.rhs.get.tpe
    val tpe2 = tree.resultTpt.toType
        
    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    // TEMPORTAL: assertChecksAndNoProblems(tree.tree.get)
  } //TASTY_QUERY BUG -> IGNORE

  // similar to -> pb011_typeparams-weird
  testSymbolWithTestlibTastyQueryContext("pb019_opaque-and-typeparams")("subtyping.TypesFromTASTy[$]") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("makeInvariantOpaque")).get.tree.get.asInstanceOf[DefDef]
    )

    val tpe1 = tree.rhs.get.tpe
    val tpe2 = tree.resultTpt.toType.widen

    //println(tree)
    //println("TYPE A: " + tpe1)
    //println("TYPE B: " + tpe2)
    assert(tpe1.isSubtype(tpe2))
    //assertChecksAndNoProblems(tree)
  } //TASTY_QUERY BUG -> SAME AS ABOVE
  */

  testSymbolWithTestlibTastyQueryContext("pb020_refinement-recursive")("simple_trees.RefinedTypeTree/T") { symbol =>
    val tree = (
      symbol.tree.get
    )
    val checker = Checker(Check.checks(List("ExprTypeConformance")))
    checker.check(tree)

    //checker.problems.foreach(x => { println(x); println() })
    assertEquals(checker.problems, List.empty[Problem])
  } //TASTY_QUERY BUG -> 0.7.4 ???

  testSymbolWithTestlibTastyQueryContext("pb022_dependent-method")("simple_trees.DependentMethod/T") { symbol =>
    val tree = (
      symbol.tree.get
    )
    val checker = Checker(Check.checks(List("ExprTypeRules")))
    checker.check(tree)

    //println("TYPE AA " + checker.problems(0).asInstanceOf[NotConformsType].a)
    
    //checker.problems.foreach(x => { println(x); println() })
    assertEquals(checker.problems, List.empty[Problem])
  } //TASTY_CHECKER BUG

  testSymbolWithTestlibTastyQueryContext("pb023_bounds-object")("simple_trees.VarargFunction/T") { symbol =>
    val tree = (
      symbol.tree.get
    )
    val checker = Checker(Check.checks(List("TypeBoundsConformance")))
    checker.check(tree)
    
    //checker.problems.foreach(x => { println(x); println() })
    assertEquals(checker.problems, List.empty[Problem])
  } //TASTY_QUERY BUG -> 0.7.8

  // ----------------

  /*
  testSymbolWithContext(TestData.test_auxiliar_context)("pb026_poly-anon-fun")("auxiliar.PotentialBugs[$]") { symbol =>
    val s = symbol.asClass.getDecl(termName("pb4")).get
    val t = s.tree.get
    println(t)
    val tp = t.asInstanceOf[ValDef].rhs.get
    println(tp.tpe)
  } //TASTY_QUERY BUG -> IGNORE
  */

  // ---

  testSymbolWithTestlibTastyQueryContext("testing_tq")("simple_trees.TypeRefIn/T") { symbol =>
    val tree1 = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("withArrayList")).get.tree
    )
    //println(tree1.get)

    val tree2 = (
      symbol.asDeclaringSymbol.asClass.getNonOverloadedDecl(termName("withArrayOfSubtypeList")).get.tree
    )
    //println(tree2.get)

    //println("XX " + tree2.get.asInstanceOf[DefDef].paramLists(1).left.get(0).asInstanceOf[ValDef].tpt.toType
    //.asInstanceOf[AppliedType].args(0).asInstanceOf[WildcardTypeBounds].bounds.high.asInstanceOf[TypeRef].optSymbol.get.asType.tree)

    val checker = Checker(Check.allChecks)
    checker.check(tree2)
    checker.problems.foreach(x => { println(x); println() })
  } //TASTY_CHECKER BUG

  // ---
  // Week 12

  testSymbolWithContext(TestData.testlib_dummy_context)("subtyping_appliedtype")("dummy.Dummy[$]") { symbol =>
    val s = symbol.asClass.getDecl(termName("tl4")).get
    val t = s.tree.get
    println(t)
    val tp = t.asInstanceOf[ValDef]
    println(tp)

    println("typcon:  " + tp.tpt.toType.asInstanceOf[AppliedType].tycon)
    // tycon returns no type parameters
    //println("typeParams:  " + tp.tpt.toType.asInstanceOf[AppliedType].tycon.typeParams)

    // we have two AppliedType in a row. the call to superType kills the typeLambdas so we loose the type params
    println(tp.rhs.get.tpe.isSubtype(tp.tpt.toType))
  }

  testSymbolWithContext(TestData.testlib_dummy_context)("subtyping_appliedtype2")("dummy.Dummy[$]") { symbol =>
    val s = symbol.asClass.getDecl(termName("tl7")).get
    val t = s.tree.get
    println(t)
    val tp = t.asInstanceOf[ValDef]
    println(tp)

    println("typcon:  " + tp.tpt.toType.asInstanceOf[AppliedType].tycon)
    // tycon returns no type parameters
    //println("typeParams:  " + tp.tpt.toType.asInstanceOf[AppliedType].tycon.typeParams)

    // we have two AppliedType in a row. the call to superType kills the typeLambdas so we loose the type params
    println(tp.rhs.get.tpe.isSubtype(tp.tpt.toType))
  }

  testSymbolWithContext(TestData.testlib_dummy_context)("wildcardbounds_TODO")("dummy.Dummy[$]") { symbol =>
    val s = symbol.asClass.getNonOverloadedDecl(termName("tp71")).get
    val t = s.tree.get
    println(t)
    
    val checker = Checker(Check.checks(List("ExprTypeConformance")))
    checker.check(t)

    //checker.problems.foreach(x => { println(x); println() })
    assertEquals(checker.problems, List.empty[Problem])
  }

  testSymbolWithContext(TestData.testlib_dummy_context)("bad_substitution")("dummy.Dummy[$]") { symbol =>
    val s1 = symbol.asClass.getNonOverloadedDecl(termName("applyAux74")).get
    val s2 = symbol.asClass.getDecl(termName("apply74")).get
    val app = s2.tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply]

    println("initial " + s1.declaredType)
    println("is subtype -> " + app.args(1).tpe.isSubtype(s1.declaredType.asInstanceOf[MethodType].instantiateParamTypes(app.args.map(_.tpe))(1)))
    s1.declaredType.asInstanceOf[MethodType].paramTypes.map{case t: TypeRef => t.optSymbol}
    println("after " + s1.declaredType)
    println("is subtype -> " + app.args(1).tpe.isSubtype(s1.declaredType.asInstanceOf[MethodType].instantiateParamTypes(app.args.map(_.tpe))(1)))

    
    //val checker = Checker(Check.checks(List("ExprTypeConformance")))
    //checker.check(t)

    //checker.problems.foreach(x => { println(x); println() })
    //assertEquals(checker.problems, List.empty[Problem])
  }
