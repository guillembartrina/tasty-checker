package tastychecker

import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Signatures.*
import tastyquery.SourceLanguage
import tastyquery.Trees.*
import tastyquery.Types.*


extension (tpe: Type)
  def isRef(sym: Symbol)(using Context): Boolean = tpe match
    case tpe: NamedType     => tpe.optSymbol.contains(sym)
    case tpe: AppliedType   => tpe.underlying.isRef(sym)
    case tpe: TermParamRef  => tpe.underlying.isRef(sym)
    case tpe: TypeParamRef  => tpe.bounds.high.isRef(sym)
    case _                  => false
  def isOfClass(cls: ClassSymbol)(using Context): Boolean = tpe match
    case tpe: TermRef       => tpe.underlying.isOfClass(cls)
    case tpe: ConstantType  => tpe.underlying.isOfClass(cls)
    case _                  => tpe.isRef(cls)
