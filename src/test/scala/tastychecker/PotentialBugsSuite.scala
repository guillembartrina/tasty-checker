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

  def testSymbolWithTestlibTastyQueryContext(testName: String)(symbolPath: String)(body: Symbol => Context ?=> Any) =
    test(testName) {
      val symbol = TestData.testlib_tastyquery_context.findSymbolFromRoot(symbolPath.split("\\.")
        .map(s =>
          if s.startsWith("&") then moduleClassName(s.stripPrefix("&"))
          else if s.startsWith("#") then typeName(s.stripPrefix("#"))
          else termName(s)).toList)
      body(symbol)(using TestData.testlib_tastyquery_context)
    }

  testSymbolWithTestlibTastyQueryContext("lambda-tpe-miss-impl")("simple_trees.#ForExpressions") { symbol =>
    println("for tree")
    println(symbol.tree.get)
    intercept[NotImplementedError] {
      val problems = Checker(Check.allChecks).check(symbol.tree.get)
    }

    // ONLY LAMBDA TPE IS MISSING, SO FAR

    //BUG TASTY_QUERY: FIXED
  }

  testSymbolWithTestlibTastyQueryContext("uninit-annot")("simple_trees.#SpecialFunctionTypes") { symbol =>
    println("special fun tree")
    println(symbol.tree.get)
    val problems = Checker(Check.allChecks).check(symbol.tree.get)

    // ONLY LAMBDA TPE IS MISSING, SO FAR

    //BUG TASTY_QUERY: FIXED
  }

  testSymbolWithTestlibTastyQueryContext("sig-toplevel-init")("<empty>.toplevelEmptyPackage$package") { symbol =>
    val tree = symbol.tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply].fun.asInstanceOf[Select]
    println("init class tree")
    println(tree)

    println("ACTUAL SIGNATURE: " +
      tree.qualifier.tpe.asInstanceOf[TypeRef]
      .optSymbol.get.asClass.declarations(2).asTerm.signedName
    )

    println("TASTY SIGNATURE: " +
      tree.name
    )
    // SHOULD BE THE SAME

    //BUG TASTY_QUERY: TO BE FIXED
  }

  testSymbolWithTestlibTastyQueryContext("sig-anon-class-constr")("simple_trees.&ScalaEnum") { symbol =>
    val tree = symbol.asDeclaringSymbol.declarations(0).tree.get.asInstanceOf[DefDef].rhs.get.asInstanceOf[Block]
    println("anon class tree")
    println(tree)

    val lookupin = (
      tree.expr.asInstanceOf[Typed]
      .expr.asInstanceOf[Apply]
      .fun.asInstanceOf[Select]
      .tpe.asInstanceOf[TermRef]
    )

    println("ACTUAL SIGNATURE: " +
      lookupin.prefix.asInstanceOf[TypeRef]
      .optSymbol.get.asClass.declarations(3).asTerm.signedName
    )
    // OR
    //.stats(0).asInstanceOf[ClassDef]
    //.rhs.constr.symbol.signature

    println("TASTY SIGNATURE: " +
      lookupin.name
    )
    // SHOULD BE THE SAME

    //BUG TASTY_QUERY: TO BE FIXED?
  }

  testSymbolWithTestlibTastyQueryContext("sig-match")("simple_trees.#MatchType") { symbol =>
    val tree = (
      symbol.asDeclaringSymbol.getDecl(termName("v")).get.tree.get.asInstanceOf[ValDef]
      .rhs.get.asInstanceOf[Apply].fun.asInstanceOf[TypeApply]
      .fun.asInstanceOf[Select]
    )
    println("init class tree")
    println(tree)

    println("ACTUAL SIGNATURE: " +
      tree.qualifier.tpe.asInstanceOf[ThisType]
      .tref.optSymbol.get.asClass.declarations(5).asTerm.signedName
    )

    println("TASTY SIGNATURE: " +
      tree.name
    )
    // SHOULD BE THE SAME
    
    //BUG TASTY_QUERY: FIXED IN 0.7.0?
  }

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

