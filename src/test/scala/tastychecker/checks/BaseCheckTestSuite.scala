package tastychecker

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Spans.* 
import tastyquery.Constants.*
import tastyquery.Trees.*
import tastyquery.Types.*


class BaseCheckTestSuite extends BaseTestSuite:
  protected val testWithTestAuxiliarContext = testWithContext(TestData.test_auxiliar_context)

  protected def assertChecks(checks: List[TreeCheck])(tree: Tree)(using Context): Unit =
    val checker = TreeChecker(checks)
    assertNoProblems(checker.check(tree))


  protected class BaseData:
    protected given Context = TestData.test_auxiliar_context

    protected val auxiliarPackage: PackageSymbol = defn.RootPackage.getPackageDecl(termName("auxiliar")).get
    protected val constructsClass: ClassSymbol = auxiliarPackage.getDecl(moduleClassName("Constructs")).get.asClass
    
    protected def getTerm(name: String): TermSymbol = constructsClass.getMember(termName(name)).get
    protected def getMeth(name: String): TermSymbol = constructsClass.getNonOverloadedDecl(termName(name)).get
    protected def getType(name: String): TypeSymbol = constructsClass.getMember(typeName(name)).get
  
    protected def decomposeValDef(t: TermSymbol): (Type, TypeTree, TermTree) =
      val tree = t.tree.get.asInstanceOf[ValDef]
      (tree.tpt.toType, tree.tpt, tree.rhs.get)

    def createIdent(t: TermSymbol): Ident = Ident(t.name)(TermRef(NoPrefix, t))(NoSpan)
    def createTypeIdent(t: TypeSymbol): TypeIdent = TypeIdent(t.name)(TypeRef(NoPrefix, t))(NoSpan)
  
    val defaultTermName: TermName = termName("termName")
    val defaultTypeName: TypeName = typeName("typeName")
    val defaultTermSymbol: TermSymbol = defn.Any_toString
    val defaultTypeTree: TypeTree = TypeIdent(typeName("Int"))(defn.IntType)(NoSpan)
    val defaultTermTree: TermTree = Literal(Constant(0))(NoSpan)
    val defaultType: Type = ConstantType(Constant(0))
    val defaultPatternTree: PatternTree = WildcardPattern(defn.IntType)(NoSpan)

    def typeTreeFrom(tpe: Type) = TypeWrapper(tpe)(NoSpan)
    def termTreeFrom(tpe: Type) = New(TypeWrapper(tpe)(NoSpan))(NoSpan)
