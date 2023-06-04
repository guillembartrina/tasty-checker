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


class LocalReferencesScopingTestSuite extends BaseCheckTestSuite:

  private object Data extends BaseData:
    val valDefTree: ValDef = getTerm("valDef").tree.get.asInstanceOf[ValDef]
    val defDefTree: DefDef = getMeth("defDef").tree.get.asInstanceOf[DefDef]
    val typeParamTree: TypeParam = defDefTree.paramLists(0).right.get(0)
    val paramValDefTree: ValDef = defDefTree.paramLists(1).left.get(0)
    val classDefTree: ClassDef = getType("classDef").tree.get.asInstanceOf[ClassDef]
    val typeMemberTree: TypeMember = getType("typeMember").tree.get.asInstanceOf[TypeMember]
    val refinedTypeTree: RefinedTypeTree = getType("refinedTypeTree").tree.get.asInstanceOf[TypeMember].rhs.asInstanceOf[TypeAliasDefinitionTree].alias.asInstanceOf[RefinedTypeTree]
    val (_, _, match0TermTree) = decomposeValDef(getTerm("match0"))
    val patternTreeWithBind: PatternTree = match0TermTree.asInstanceOf[Match].cases(0).pattern
    val bindTree = patternTreeWithBind.asInstanceOf[Bind]
    //val patternTreeWithTypeBind: PatternTree = match0TermTree.asInstanceOf[Match].cases(1).pattern
    //val typeBindTree = patternTreeWithTypeBind.asInstanceOf[TypeTest].tpt.asInstanceOf[AppliedTypeTree].args(0).asInstanceOf[TypeTreeBind]
    //TODO: Quoted pattern match, TypeBindingsTree
    val (_, typeMatchTypeTree, _) = decomposeValDef(getTerm("typeMatch"))
    val typeTreeWithTypeBind: TypeTree = typeMatchTypeTree.asInstanceOf[MatchTypeTree].cases(0).pattern
    val typeBindTree2 = typeTreeWithTypeBind.asInstanceOf[AppliedTypeTree].args(0).asInstanceOf[TypeTreeBind]

  import Data.*

  private def assertChecksLocalReferencesScoping(using Context) = assertChecks(List(LocalReferencesScoping))

  private def assertProblemsLocalReferencesScope(tree: Tree, problems: List[Problem])(using Context) =
    val checker = TreeChecker(List(LocalReferencesScoping))
    assertProblems(checker.check(tree), problems)

  private def createNotInScope(symbols: List[TermOrTypeSymbol]): List[Problem] =
    symbols.map{
      case ts: TermSymbol => NotInScope(ts, TermRef(NoPrefix, ts))
      case ts: TypeSymbol => NotInScope(ts, TypeRef(NoPrefix, ts))
    }

  testWithTestAuxiliarContext("tree_Block") {
    assertChecksLocalReferencesScoping(
      Block(List(
        valDefTree,
        createIdent(valDefTree.symbol),
        defDefTree,
        createIdent(defDefTree.symbol),
        classDefTree,
        New(createTypeIdent(classDefTree.symbol))(NoSpan),
        typeMemberTree,
        New(createTypeIdent(typeMemberTree.symbol))(NoSpan),
      ),
      defaultTermTree)(NoSpan)
    )

    assertProblemsLocalReferencesScope(
      Block(List(
        createIdent(valDefTree.symbol),
        createIdent(defDefTree.symbol),
        New(createTypeIdent(classDefTree.symbol))(NoSpan),
        New(createTypeIdent(typeMemberTree.symbol))(NoSpan),
      ),
      defaultTermTree)(NoSpan),
      createNotInScope(List(
        valDefTree.symbol, defDefTree.symbol, classDefTree.symbol, typeMemberTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_Inlined") {
    assertChecksLocalReferencesScoping(
      Inlined(
        Block(List(
          createIdent(valDefTree.symbol),
          createIdent(defDefTree.symbol)
        ), defaultTermTree)(NoSpan),
        None, List(
          valDefTree,
          defDefTree,
      ))(NoSpan)
    )
    assertProblemsLocalReferencesScope(
      Inlined(
        Block(List(
          createIdent(valDefTree.symbol),
          createIdent(defDefTree.symbol)
        ), defaultTermTree)(NoSpan),
      None, Nil)(NoSpan),
      createNotInScope(List(
        valDefTree.symbol, defDefTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_DefDef") {
    assertChecksLocalReferencesScoping(
      DefDef(
        defaultTermName,
        List(
          Left(
            List(
              valDefTree,
              ValDef(defaultTermName, defaultTypeTree, Some(createIdent(valDefTree.symbol)), defaultTermSymbol)(NoSpan)
          )),
          Right(
            List(
              typeParamTree
          )),
          Left(
            List(
              ValDef(defaultTermName, createTypeIdent(typeParamTree.symbol), Some(createIdent(valDefTree.symbol)), defaultTermSymbol)(NoSpan)
          ))
        ),
        createTypeIdent(typeParamTree.symbol),
        Some(createIdent(valDefTree.symbol)),
        defaultTermSymbol
      )(NoSpan)
    )
    assertProblemsLocalReferencesScope(
      DefDef(
        defaultTermName,
        List(
          Left(
            List(
              ValDef(defaultTermName, defaultTypeTree, Some(createIdent(valDefTree.symbol)), defaultTermSymbol)(NoSpan)
          )),
          Left(
            List(
              ValDef(defaultTermName, createTypeIdent(typeParamTree.symbol), Some(createIdent(valDefTree.symbol)), defaultTermSymbol)(NoSpan)
          ))
        ),
        createTypeIdent(typeParamTree.symbol),
        Some(createIdent(valDefTree.symbol)),
        defaultTermSymbol
      )(NoSpan),
      createNotInScope(List(
        valDefTree.symbol, typeParamTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_TypeLambdaTree") {
    assertChecksLocalReferencesScoping(
      TypeLambdaTree(
        List(typeParamTree),
        createTypeIdent(typeParamTree.symbol)
      )(NoSpan)
    )
    assertProblemsLocalReferencesScope(
      TypeLambdaTree(
        Nil,
        createTypeIdent(typeParamTree.symbol)
      )(NoSpan),
      createNotInScope(List(
        typeParamTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_PolyTypeDefinitionTree") {
    assertChecksLocalReferencesScoping(
      PolyTypeDefinitionTree(
        List(typeParamTree),
        TypeAliasDefinitionTree(createTypeIdent(typeParamTree.symbol))(NoSpan)
      )(NoSpan)
    )
    assertProblemsLocalReferencesScope(
      PolyTypeDefinitionTree(
        Nil,
        TypeAliasDefinitionTree(createTypeIdent(typeParamTree.symbol))(NoSpan)
      )(NoSpan),
      createNotInScope(List(
        typeParamTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_RefinedTypeTree") {
    /*
    assertChecksLocalReferencesScope(
      RefinedTypeTree(
        defaultTypeTree,
        List(
          ValDef(defaultTermName, createTypeIdent(refinedTypeTree.refinedCls), None, defaultTermSymbol)(NoSpan)
        ),
        refinedTypeTree.refinedCls
      )(NoSpan)
    )
    */
    assertProblemsLocalReferencesScope(
      RefinedTypeTree(
        defaultTypeTree,
        List(
          ValDef(defaultTermName, createTypeIdent(typeParamTree.symbol), None, defaultTermSymbol)(NoSpan)
        ),
        refinedTypeTree.refinedCls
      )(NoSpan),
      createNotInScope(List(
        typeParamTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_CaseDef") {
    assertChecksLocalReferencesScoping(
      CaseDef(
        patternTreeWithBind,
        Some(createIdent(bindTree.symbol)),
        createIdent(bindTree.symbol)
      )(NoSpan)
    )
    assertProblemsLocalReferencesScope(
      CaseDef(
        defaultPatternTree,
        Some(createIdent(bindTree.symbol)),
        createIdent(bindTree.symbol)
      )(NoSpan),
      createNotInScope(List(
        bindTree.symbol
      ))
    )
  }

  testWithTestAuxiliarContext("tree_TypeCaseDef") {
    assertChecksLocalReferencesScoping(
      TypeCaseDef(
        typeTreeWithTypeBind,
        createTypeIdent(typeBindTree2.symbol)
      )(NoSpan)
    )
    assertProblemsLocalReferencesScope(
      TypeCaseDef(
        defaultTypeTree,
        createTypeIdent(typeBindTree2.symbol)
      )(NoSpan),
      createNotInScope(List(
        typeBindTree2.symbol
      ))
    )
  }

