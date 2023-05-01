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

class DummyTestSuite extends BaseTestSuite:

  testSymbolWithContext(TestData.testlib_dummy_context)("dummy")("dummy.TypeParamBoundsConformance[$]") { symbol =>
    val s = symbol.asClass.getDecl(termName("tp6")).get
    val t = s.tree.get
    println("TREE " + t + "\n")
    val tp = t.asInstanceOf[ValDef].rhs.get
    println("Type " + tp.tpe)

    //val tp2 = t.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply].fun
    //println(tp2.tpe)
    //println(tp2.tpe.widen)

    //val tp2 = t.asInstanceOf[ValDef].rhs.get.asInstanceOf[Block].expr.asInstanceOf[Assign].lhs
    //println(tp2.tpe)
    //println(tp2.tpe.widen)

    //val s = symbol.asClass.getNonOverloadedDecl(termName("inlineMatchAux")).get
    //val t = s.tree.get
    //println(t)
    //val tp1 = t.asInstanceOf[DefDef].rhs.get.tpe
    //println(tp1)
    //val tp2 = s.declaredType
    //println(tp2)

    val checker = Checker(Check.checks(List("TypeBoundsConformance")))
    checker.check(t)
    println("PROBLEMS:\n" + checker.problems)
  }

  /*
  testSymbolWithContext(TestData.testlib_dummy_context)("dummy")("dummy.ExpressionTypeConformance[$]") { symbol =>
    val tree = symbol.asClass.getDecl(termName("pat")).get
    //val tree = symbol.asClass.getDecl(termName("test")).get.tree.get
    //val tree = symbol.asClass.getNonOverloadedDecl(termName("ss")).get
    //val aux = tree.asInstanceOf[ValDef].rhs.get.asInstanceOf[Block].expr
    println(tree.tree)
    //println(aux.tpe.widen)
  }
  */

  /*
  testSymbolWithContext(TestData.testlib_dummy_context)("dummy0")("dummy.LSP[$].a4") { symbol =>
    val tree = symbol.tree.get.asInstanceOf[ValDef].rhs.get.asInstanceOf[Apply]
    println(tree.args.map(_.tpe))
    println(tree.fun.tpe.widen)
    println(defn.FunctionNClass(tree.args.size).staticRef.appliedTo(tree.args.map(_.tpe.widen) :+ tree.tpe))
  }
  */