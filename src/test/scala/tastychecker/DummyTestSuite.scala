package tastychecker

import tastyquery.*
import tastyquery.Contexts.*
import tastyquery.Exceptions.*
import tastyquery.Spans.*
import tastyquery.Names.*
import tastyquery.Flags.*
import tastyquery.Symbols.*
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*


@munit.IgnoreSuite
class DummyTestSuite extends BaseTestSuite:

  testSymbolWithContext(TestData.testlib_dummy_context)("dummy")("dummy.ExprTypeConformance[$]") { symbol =>
    //val s = symbol.asClass.getDecl(termName("xxx")).get
    //val s = symbol.asClass.getNonOverloadedDecl(termName("castMatchResultWithBind")).get
    val s = symbol.asClass.getNonOverloadedDecl(SimpleName("defDef1"))

    println(s)

    //println(s.foreach(x => println(" " + x.declaredType + "\n\t" + x.asTerm.annotations)))
    //println(s.foreach(x => println(" " + x.asTerm.allOverriddenSymbols.toList)))

    //val t = s.tree.get
    //println("TREE " + t + "\n")
    //t.asInstanceOf[DefDef].rhs.get.asInstanceOf[TypeApply].fun.asInstanceOf[Select].qualifier.tpe.print

    //println("K >" + t.asInstanceOf[DefDef].rhs.get.reduce(x => println(" " + x + " "))((x, y) => ()))

    //println(t.asInstanceOf[TypeMember].rhs.asInstanceOf[PolyTypeDefinitionTree].body.asInstanceOf[TypeAliasDefinitionTree].alias
    //.asInstanceOf[MatchTypeTree].cases(0).pattern.toType)

    //ValDef
    //val tp = t.asInstanceOf[ValDef]
    //println("rhs:  " + tp.rhs.get.tpe)
    //println("type rhs:  " + tp.rhs.get.tpe)
    //println("tpe:  " + tp.tpt.toType)
    //println("tpe:  " + tp.tpt.toType.asInstanceOf[AppliedType].superType)
    //println("tpe:  " + tp.rhs.get.tpe.isSubtype(tp.tpt.toType))

    //DefDef
    //val tp = t.asInstanceOf[DefDef]
    //println("rhs:  " + tp.rhs.get.tpe)
    //println(tp.rhs.get)

    //>Block
    //val tpp = tp.rhs.get.asInstanceOf[Block].expr
    //println("TYPE " + tpp.tpe)

    //val tpp2 = tp.rhs.get.asInstanceOf[Block].stats(1).asInstanceOf[ValDef]
    //println(" " + tpp2.tpt.toType + tpp2.rhs.get.asInstanceOf[Apply].fun.tpe)

    //>Apply
    //val tpp = tp.rhs.get.asInstanceOf[Apply].fun
    //println("TYPE " + tpp.tpe)
    //println(tpp.asInstanceOf[Select].qualifier.asInstanceOf[Ident].tpe)

    //>Match
    //val tpp = tp.rhs.get.asInstanceOf[Match]
    //println("selector: " + tpp.selector.tpe.widen)
    //for c <- tpp.cases do
    //  println("[case] " + c.pattern) // + " # " + c.body.asInstanceOf[Block].expr.asInstanceOf[Typed].expr.asInstanceOf[Ident].tpe)
    //  //c.pattern.print
    //  //println(c.pattern.asInstanceOf[TypeTest].body.asInstanceOf[Bind].symbol.declaredType)

    //tp.rhs.get.asInstanceOf[Match].cases(0).print

    //val m1 = symbol.asClass.getDecl(typeName("X1")).get.asClass.getNonOverloadedDecl(termName("mem2")).get
    //val m2 = symbol.asClass.getDecl(typeName("X2")).get.asClass.getNonOverloadedDecl(termName("mem2")).get
    //println(m2.signature == m1.signature)
    //println(m2.declaredType.matches(m1.declaredType))
    //println(fromType(m1.declaredType.asSeenFrom(m2.owner.asClass.thisType, m1.owner), m1.sourceLanguage))
    //println(m2.signature)
    //println(m1.signedName == m2.signedName)
    //println(ErasedTypeRef.erase(m1.declaredType))
    //println(ErasedTypeRef.erase(m2.declaredType))

    //println(s.is(Method))
    //println(s.declaredType)
    //println(s.sourceLanguage)
    //println(s.nextOverriddenSymbol.get.asTerm.declaredType)
    //println("---------")
    //println(s.allOverriddenSymbols.toList.foreach(x => println("> " + x.asTerm + " " + x.asTerm.declaredType)))
    //println("---------")
    //println(s.allOverriddenSymbols.toList.foreach(x => println("> " + x.asTerm + " " + x.asTerm.sourceLanguage)))

    val checker = TreeChecker(TreeCheck.someChecks("LocalReferencesScope" :: Nil), TreeFilter.empty)
    println("PROBLEMS:\n" + checker.check(symbol.tree.get))
  }
