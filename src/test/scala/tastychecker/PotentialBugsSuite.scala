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

  /*
  testSymbolWithTestlibTastyQueryContext("pb001_lambda-tpe-miss-impl")("simple_trees.ForExpressions/T") { symbol =>
    val problems = Checker(Check.allChecks).check(symbol.tree.get)
    //println(symbol.tree.get)
  } //TASTY_QUERY TODO -> IMPLEMENTED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb002_uninit-annot")("simple_trees.SpecialFunctionTypes/T") { symbol =>
    val problems = Checker(Check.allChecks).check(symbol.tree.get)
    //println(symbol.tree.get)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb003_sig-toplevel-init")("<empty>.toplevelEmptyPackage$package") { symbol =>
    val tree = symbol.tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select]

    val sig1 = tree.qualifier.tpe.asInstanceOf[TypeRef].optSymbol.get.asClass.declarations(2).asTerm.signedName
    val sig2 = tree.name
    
    assertEquals(sig1, sig2)
    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb004_sig-anon-class-constr")("simple_trees.&ScalaEnum") { symbol =>
    val tree = symbol.asDeclaringSymbol.declarations(0).tree.get.asInstanceOf[DefDef].rhs.get.asInstanceOf[Block]

    val lookupin = (
      tree.expr.asInstanceOf[Typed]
      .expr.asInstanceOf[Apply]
      .fun.asInstanceOf[Select]
      .tpe.asInstanceOf[TermRef]
    )

    val sig1 = lookupin.prefix.asInstanceOf[TypeRef].optSymbol.get.asClass.declarations(3).asTerm.signedName
    val sig2 = lookupin.name

    assertEquals(sig1, sig2)
    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
  } //TASTY_QUERY BUG -> FIXED 0.7.1

  testSymbolWithTestlibTastyQueryContext("pb005_sig-match")("simple_trees.#MatchType") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("v")).get.tree.get.asInstanceOf[ValDef]
      .rhs.get.asInstanceOf[Apply].fun.asInstanceOf[TypeApply]
      .fun.asInstanceOf[Select]
    )

    val sig1 = tree.qualifier.tpe.asInstanceOf[ThisType].tref.optSymbol.get.asClass.declarations(5).asTerm.signedName
    val sig2 = tree.name

    assertEquals(sig1, sig2)
    //println(tree)
    //println("ACTUAL SIGNATURE: " + sig1)
    //println("TASTY SIGNATURE: " + sig2)
  } //TASTY_QUERY BUG -> FIXED 0.7.0

  testSymbolWithTestlibTastyQueryContext("subtyping-seq-and-repeated")("simple_trees.#TypeApply") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("x")).get.tree.get.asInstanceOf[ValDef]
      .rhs.get.asInstanceOf[Apply]
      .args(0).asInstanceOf[Typed]
    )
    println("seq apply tree")
    println(tree)
    println("TYPE A: " + tree.expr.tpe)
    println("TYPE B: " + tree.tpt)
    // SHOULD SUBTYPE

    //BUG TASTY_QUERY: TO BE FIXED
  }

  testSymbolWithTestlibTastyQueryContext("wrong-resolution-bitset")("simple_trees.#Repeated") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.declarations(1).tree.get.asInstanceOf[DefDef]
    )
    println("seq apply tree")
    println(tree)

    println("Tree A: " + tree.rhs.get.asInstanceOf[Apply].fun)
    println("TYPE A: " + tree.rhs.get.tpe)
    println("TYPE B: " + tree.resultTpt.toType.widen)

    println(tree.rhs.get.asInstanceOf[Apply].fun.tpe.asInstanceOf[TermRef].underlying)

    // SHOULD RESOLVE TO BitSet

    import scala.collection.mutable.BitSet
    val x = BitSet(1, 2, 3)

    //Symbols.scala - 1035, do typeparams need to be resolved?

    //BUG TASTY_QUERY: TO BE FIXED
  }

  testSymbolWithTestlibTastyQueryContext("wrong-resolution-function1")("simple_trees.#QualThisType.#Inner") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.declarations(1).tree.get.asInstanceOf[DefDef]
    )
    println("seq apply tree")
    println(tree)

    println("Tree A: " + tree.rhs.get.asInstanceOf[Apply].fun)
    println("TYPE A: " + tree.rhs.get.tpe)
    println("TYPE B: " + tree.resultTpt.toType.widen)

    // SHOULD RESOLVE TO Unit

    val tree2 = (
      symbol.asDeclaringSymbol.declarations(1).tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply]
    )
    println("seq apply tree")
    println(tree)

    println("Tree A: " + tree2.args(0))
    println("TYPE A: " + tree2.args(0).tpe)
    println("TYPE B: " + tree2.fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0))

    Function1

    //Symbols.scala - 1035, do typeparams need to be resolved?

    //BUG TASTY_QUERY: TO BE FIXED
  }

  testSymbolWithTestlibTastyQueryContext("byname-wrong-subtyping")("simple_trees.#OverloadedApply") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("callE")).get.tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply]
    )
    println("byname tree")
    println(tree)

    println("TYPE A: " + tree.args(0).tpe)
    println("TYPE B: " + tree.fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0))

    // SHOULD SUBTYPE

    //BUG TASTY_CHECKER: TO BE FIXED
  }

  testSymbolWithTestlibTastyQueryContext("byname-wrong-subtyping2")("simple_trees.#RefinedTypeTree") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("a")).get.tree.get.asInstanceOf[ValDef]
    )
    println("byname tree 2")
    println(tree)

    println("TYPE A: " + tree.rhs.get.tpe)
    println("TYPE B: " + tree.tpt.toType)

    // SHOULD SUBTYPE
    //BUG TASTY_QUERY: FIXED IN 0.7.0?
  }

  testSymbolWithTestlibTastyQueryContext("typeparams-weird")("simple_trees.#TypeRefIn") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.declarations(1).tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply]
    )
    println("typeparams tree")
    println(tree)

    println("TYPE A: " + tree.args(0).tpe.widen)
    println("TYPE B: " + tree.fun.tpe.widen.asInstanceOf[MethodType].paramTypes(0))

    // SHOULD SUBTYPE

    //BUG TASTY_QUERY: POSTPONE
  }

  testSymbolWithTestlibTastyQueryContext("type-aliases")("simple_trees.#AnyMethods") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.declarations(10).tree.get.asInstanceOf[DefDef]
    )
    println("typeparams tree")
    println(tree)

    println("TYPE A: " + tree.rhs.get.tpe)
    println("TYPE B: " + tree.resultTpt.toType)

    // SHOULD SUBTYPE

    //BUG COMPILER
  }
  */

  // ----------------

  testSymbolWithTestlibTastyQueryContext("sig-tlota")("crosspackagetasty") { symbol =>

    val x = symbol.asDeclaringSymbol.declarations(2).asDeclaringSymbol.declarations(1).tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select]
    println("TASTY SIGNATURE: " + x.name)

    val y = symbol.asDeclaringSymbol.declarations(2).asDeclaringSymbol.declarations(0).asDeclaringSymbol.declarations(2).asTerm.signedName
    println("ACTUAL SIGNATURE: " + y)


    val checker = Checker(Check.checks(List("LSP")))
    checker.check(symbol.asDeclaringSymbol.declarations(2).tree)

    //Signatures strike again
  }

  testSymbolWithTestlibTastyQueryContext("weird-type")("simple_trees.MatchType/T") { symbol =>

    val checker = Checker(Check.checks(List("LSP")))
    checker.check(symbol.asDeclaringSymbol.declarations(9).tree)
    checker.problems.foreach(x => { println(x); println() })

    //Match types
  }

  testSymbolWithTestlibTastyQueryContext("refined-types")("simple_trees.RefinedTypeTree/T") { symbol =>

    val x = symbol.asDeclaringSymbol.declarations(12).tree.get.asInstanceOf[DefDef]
      .rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select]
    println(x.name)
    println("Tree: " + x.qualifier.tpe.widen)
    println(x.qualifier.tpe.widen.asInstanceOf[TypeRef].optSymbol.get.asDeclaringSymbol.declarations(1).asTerm.signedName)


    val checker = Checker(Check.checks(List("LSP")))
    checker.check(symbol.asDeclaringSymbol.declarations(12).tree)
    checker.problems.foreach(x => { println(x); println() })

    //Member of refined type not found
  }

  testSymbolWithTestlibTastyQueryContext("applied-types")("simple_trees.ForExpressions/T") { symbol =>

    val checker = Checker(Check.checks(List("LSP")))
    checker.check(symbol.tree)
    checker.problems.foreach(x => { println(x); println() })

    //Special typeapplies
  }

  testSymbolWithTestlibTastyQueryContext("functions")("simple_trees.Function/T") { symbol =>

    val checker = Checker(Check.checks(List("LSP")))
    checker.check(symbol.tree)
    checker.problems.foreach(x => { println(x); println() })

    //Types are very similar or equal
  }

  /*
  testSymbolWithTestlibTastyQueryContext("part-functions")("simple_trees.WithPartialFunction/T") { symbol =>

    val checker = Checker(Check.checks(List("LSP")))
    checker.check(symbol.tree)
    
    //println(checker.problems(0).tree.asInstanceOf[Lambda].meth.tpe.widen)

    checker.problems.foreach(x => { println(x); println() })

    //Looks like the type of meth is not correct? Or maybe isSubtype doesn't work for SAMs
  }
  */
