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


//Note: This TestSuite is not complete because this check is quite complex
class MatchingTypeCorrectnessTestSuite extends BaseCheckTestSuite:

  private object Data extends BaseData:

    val booleanFull @ (booleanType, booleanTypeTree, booleanTermTree) = decomposeValDef(getTerm("boolean"))
    val _falseFull @ (_falseType, _falseTypeTree, _falseTermTree) = decomposeValDef(getTerm("_false"))
    val stringFull @ (stringType, stringTypeTree, stringTermTree) = decomposeValDef(getTerm("string"))

    val booleanTermSymbol = getTerm("boolean")
    val booleanXTermSymbol = getTerm("booleanX")
    val stringTermSymbol = getTerm("string")

    val seqBooleanFull @ (seqBooleanType, seqBooleanTypeTree, seqBooleanTermTree) = decomposeValDef(getTerm("seqBoolean"))
    val repeatedBooleanFull @ (repeatedType, repeatedTypeTree, repeatedTermTree) = (
      defn.RepeatedTypeOf(booleanType),
      AppliedTypeTree(TypeIdent(Names.typeName("<repeated>"))(defn.RepeatedTypeUnapplied)(NoSpan), List(booleanTypeTree))(NoSpan),
      Typed(SeqLiteral(List(Literal(Constant(0))(NoSpan)), booleanTypeTree)(NoSpan),
        AppliedTypeTree(TypeIdent(Names.typeName("<repeated>"))(defn.RepeatedTypeUnapplied)(NoSpan), List(booleanTypeTree))(NoSpan))(NoSpan)
    )

    val booleanAndStringTermSymbol = getTerm("booleanAndString")
    val booleanOrStringTermSymbol = getTerm("booleanOrString")

    val (_, _, product1BooleanTermTree) = decomposeValDef(getTerm("unapplyProduct1BooleanRef"))
    val funBooleanToProduct1Boolean = product1BooleanTermTree.asInstanceOf[Apply].fun

    val (_, _, product1BooleanImplicitsTermTree) = decomposeValDef(getTerm("unapplyProduct1BooleanImplicitsRef"))
    val funBooleanToProduct1BooleanImplicits = product1BooleanImplicitsTermTree.asInstanceOf[Apply].fun.asInstanceOf[Apply].fun

    val (_, _, singleMatchTermTree) = decomposeValDef(getTerm("unapplySingleMatchRef"))
    val funSingleMatch = singleMatchTermTree.asInstanceOf[Apply].fun

    val (_, _, nameBasedTermTree) = decomposeValDef(getTerm("unapplyNameBasedRef"))
    val funNameBased = nameBasedTermTree.asInstanceOf[Apply].fun

  import Data.*

  private def assertChecksMatchingTypeCorrectness(using Context) = assertChecks(List(MatchingTypeCorrectness))

  testWithTestAuxiliarContext("assumptions") {
    assert(booleanType.isSameType(booleanType))
    assert(stringType.isSameType(stringType))
    assert(!booleanType.isSameType(stringType))
    assert(!stringType.isSameType(booleanType))

    assert(booleanTermSymbol.declaredType.isSameType(booleanType))
    assert(stringTermSymbol.declaredType.isSameType(stringType))
    assert(!booleanTermSymbol.declaredType.isSameType(stringType))
    assert(!stringTermSymbol.declaredType.isSameType(booleanType))
  }

  private def exhaustiveTests(builder: (TermTree, PatternTree) => Span => Tree)(using Context): Unit =
    val checker = TreeChecker(List(MatchingTypeCorrectness))
    def noProblem(t: TermTree, p: PatternTree) =
      assertNoProblems(checker.check(builder(t, p)(NoSpan)))
    def problem(t: TermTree, p: PatternTree, z: Problem) =
      assertProblems(checker.check(builder(t, p)(NoSpan)), List(z))
    def enforces(t: Type): PatternTree = WildcardPattern(t)(NoSpan)
    def anything: PatternTree = ExprPattern(defaultTermTree)(NoSpan)
    def captures(t: TermSymbol, p: PatternTree) = Bind(defaultTermName, p, t)(NoSpan)
    //ExprPatter
    noProblem(defaultTermTree, ExprPattern(defaultTermTree)(NoSpan))
    //WildcardPattern
    noProblem(booleanTermTree, WildcardPattern(booleanType)(NoSpan))
    val t1 = WildcardPattern(booleanType)(NoSpan)
    problem(stringTermTree, t1, NotMatchesType(booleanType, stringType, t1))
    //Bind
    noProblem(booleanTermTree, Bind(defaultTermName, enforces(booleanType), booleanTermSymbol)(NoSpan))
    val t2 = Bind(defaultTermName, enforces(booleanType), stringTermSymbol)(NoSpan)
    problem(booleanTermTree, t2, NotMatchesType(booleanType, stringType, t2))
    noProblem(booleanTermTree, captures(booleanXTermSymbol, Bind(defaultTermName, enforces(booleanType), booleanTermSymbol)(NoSpan)))
    val t3 = captures(stringTermSymbol, Bind(defaultTermName, enforces(booleanType), booleanTermSymbol)(NoSpan))
    problem(booleanTermTree, t3, NotMatchesType(TermRef(NoPrefix, booleanTermSymbol), stringTermSymbol.declaredType, t3))
    //TypeTest
    noProblem(defaultTermTree, TypeTest(enforces(booleanType), booleanTypeTree)(NoSpan))
    problem(defaultTermTree, TypeTest(enforces(booleanType), stringTypeTree)(NoSpan),
      NotMatchesType(booleanType, stringType, enforces(booleanType)))
    noProblem(defaultTermTree, TypeTest(enforces(seqBooleanType), repeatedTypeTree)(NoSpan))
    noProblem(stringTermTree, captures(booleanAndStringTermSymbol, TypeTest(anything, booleanTypeTree)(NoSpan)))
    val t4 = captures(booleanAndStringTermSymbol, TypeTest(anything, booleanTypeTree)(NoSpan))
    problem(booleanTermTree, t4, NotMatchesType(AndType(booleanType, booleanType), booleanAndStringTermSymbol.declaredType, t4))
    //Alternative
    noProblem(defaultTermTree, Alternative(List(anything, anything))(NoSpan))
    noProblem(booleanTermTree, captures(booleanTermSymbol, Alternative(List(enforces(booleanType), enforces(booleanType)))(NoSpan)))
    val t5 = captures(stringTermSymbol, Alternative(List(enforces(booleanType), enforces(booleanType)))(NoSpan))
    problem(booleanTermTree, t5, NotMatchesType(OrType(booleanType, booleanType), stringType, t5))
    //Unapply
    noProblem(booleanTermTree, Unapply(funBooleanToProduct1Boolean, Nil, List(enforces(booleanType)))(NoSpan))
    val t6 = WildcardPattern(stringType)(NoSpan)
    problem(booleanTermTree, Unapply(funBooleanToProduct1Boolean, Nil, List(t6))(NoSpan), NotMatchesType(stringType, booleanType, t6))
    noProblem(booleanTermTree, Unapply(funBooleanToProduct1BooleanImplicits, List(booleanTermTree), List(enforces(booleanType)))(NoSpan))
    val t7 = WildcardPattern(stringType)(NoSpan)
    problem(booleanTermTree, Unapply(funBooleanToProduct1BooleanImplicits, List(booleanTermTree), List(t7))(NoSpan), NotMatchesType(stringType, booleanType, t7))
    noProblem(booleanTermTree, Unapply(funSingleMatch, Nil, List(enforces(booleanType)))(NoSpan))
    val t8 = WildcardPattern(stringType)(NoSpan)
    problem(booleanTermTree, Unapply(funSingleMatch, Nil, List(t8))(NoSpan), NotMatchesType(stringType, booleanType, t8))
    noProblem(booleanTermTree, Unapply(funNameBased, Nil, List(enforces(booleanType), enforces(booleanType)))(NoSpan))
    val t9 = WildcardPattern(stringType)(NoSpan)
    problem(booleanTermTree, Unapply(funNameBased, Nil, List(enforces(booleanType), t9))(NoSpan), NotMatchesType(stringType, booleanType, t9))

  testWithTestAuxiliarContext("tree_Match") {
    exhaustiveTests{ (sel, patt) =>
      Match(sel, List(CaseDef(patt, None, defaultTermTree)(NoSpan)))
    }
    assertChecksMatchingTypeCorrectness(Match(defaultTermTree, Nil)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_InlineMatch") {
    exhaustiveTests{ (sel, patt) =>
      InlineMatch(Some(sel), List(CaseDef(patt, None, defaultTermTree)(NoSpan)))
    }
    assertChecksMatchingTypeCorrectness(InlineMatch(None, Nil)(NoSpan))
  }

  testWithTestAuxiliarContext("tree_Match") {
    //TODO: analogous check but with Throwable selector
    assertChecksMatchingTypeCorrectness(Try(defaultTermTree, Nil, None)(NoSpan))
  }
