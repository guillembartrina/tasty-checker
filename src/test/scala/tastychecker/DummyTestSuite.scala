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

  testSymbolWithContext(TestData.testlib_dummy_context)("dummy")("dummy.Dummy[$]") { symbol =>
    //val s = symbol.asClass.getDecl(termName("xxx")).get
    val s = symbol.asClass.getNonOverloadedDecl(termName("applyAux74")).get

    val t = s.tree.get
    println("TREE " + t + "\n")

    //ValDef
    //val tp = t.asInstanceOf[ValDef]
    //println("rhs:  " + tp.rhs.get)
    //println("type rhs:  " + tp.rhs.get.tpe)
    //println("tpe:  " + tp.tpt.toType)
    //println("tpe:  " + tp.tpt.toType.asInstanceOf[AppliedType].superType)
    //println("tpe:  " + tp.rhs.get.tpe.isSubtype(tp.tpt.toType))

    //DefDef
    //val tp = t.asInstanceOf[DefDef]
    //println("rhs:  " + tp.rhs.get.tpe)

    //>Block
    //val tpp = tp.rhs.get.asInstanceOf[Block].expr
    //println("TYPE " + tpp.tpe)

    //>Apply
    //val tpp = tp.rhs.get.asInstanceOf[Apply].fun
    //println("TYPE " + tpp.tpe)

    //>Match
    //val tpp = tp.rhs.get.asInstanceOf[Match]
    //println("selector: " + tpp.selector.tpe.widen)
    //for c <- tpp.cases do
    //  println("[case] " + c.pattern + " # " + c.body.tpe)
    //  //c.pattern.print
    //  //println(c.pattern.asInstanceOf[TypeTest].body.asInstanceOf[Bind].symbol.declaredType)

    val checker = Checker(Check.allChecks, currentFilter)
    checker.check(symbol.tree.get)
    println("PROBLEMS:\n" + checker.problems)
  }

  /*
  testSymbolWithContext(TestData.testlib_dummy_context)("dummy1")("dummy.TypeMemberBoundsConformance[$].TMBL/T") { symbol =>
    println(Check.checks("TypeMemberBoundsConformance" :: Nil))
    println(symbol.tree.get)
    val checker = Checker(Check.checks("TypeMemberBoundsConformance" :: Nil))
    checker.check(symbol.tree.get)
    println("PROBLEMS:\n" + checker.problems)
  }
  */
