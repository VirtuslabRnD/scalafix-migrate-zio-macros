package org.virtuslab.rewrites

import scala.collection.mutable
import scalafix.v1._
import scala.meta._
import scalafix.patch.Patch

trait ZIOCodeGen { self: SemanticRule =>

  object DefDef {
    def unapply(t: Tree) = t match {
      case t: Defn.Def => Some(t)
      case t: Decl.Def => Some(t)
      case _           => None
    }
  }
  object ValDef {
    def unapply(t: Tree) = t match {
      case t: Defn.Val => Some(t)
      case t: Decl.Val => Some(t)
      case _           => None
    }
  }

  type PatchesBuilder = mutable.Builder[Patch, Seq[Patch]]
  object symbols {
    val MockableAnnotation = Symbol("zio/mock/mockable#")
    val AccessibleAnnotation = Symbol("zio/macros/accessible#")

    val ZIO = Symbol("zio/ZIO#")
    val ZIOIO = Symbol("zio/package.IO#")
    val ZIOTask = Symbol("zio/package.Task#")
    val ZIORIO = Symbol("zio/package.RIO#")
    val ZIOUIO = Symbol("zio/package.UIO#")
    val ZIOURIO = Symbol("zio/package.URIO#")

    val ZIOZSink = Symbol("zio/stream/ZSink#")
    val ZIOSink = Symbol("zio/stream/package.Sink#")
    val ZIOZStream = Symbol("zio/stream/ZStream#")
    val ZIOStream = Symbol("zio/stream/package.Stream#")
    val ZIOUStream = Symbol("zio/stream/package.UStream#")

    val ZIOZManaged = Symbol("zio/managed/ZManaged#")
    val ZIOManaged = Symbol("zio/managed/package.Managed#")
    val ZIOTaskManaged = Symbol("zio/managed/package.TaskManaged#")
    val ZIORManaged = Symbol("zio/managed/package.RManaged#")
    val ZIOUManaged = Symbol("zio/managed/package.UManaged#")
    val ZIOURManaged = Symbol("zio/managed/package.URManaged#")

    val ZIOMock = Symbol("zio/mock/Mock#")
    val IzumiReflectTag = Symbol("izumi/reflect/Tag#")

    val Unit = Symbol("scala/Unit#")
    val Nothing = Symbol("scala/Nothing#")
    val Any = Symbol("scala/Any#")
    val Throwable = Symbol("java/lang/Throwable#")

    implicit class SymbolOps(val self: Symbol) {
      def is(v: Symbol): Boolean = self.value == v.value
    }
  }
  import symbols.SymbolOps
  object types {
    def termName(sym: Symbol): Term.Ref = {
      if (sym.isNone) sys.error(sym.toString())
      else if (sym.owner.isRootPackage || sym.owner.isEmptyPackage) Term.Name(sym.displayName)
      else Term.Select(termName(sym.owner), Term.Name(sym.displayName))
    }

    def fromSymbol(sym: Symbol)(implicit doc: Symtab): Type =
      if (sym.info.exists(t => t.isTypeParameter)) Type.Name(sym.displayName)
      else if (sym.owner.isEmptyPackage || sym.owner.isRootPackage) Type.Name(sym.displayName)
      else Type.Select(termName(sym.owner), Type.Name(sym.displayName))
    // Type.Name(sym.displayName)

    def Unit(implicit doc: Symtab) = fromSymbol(symbols.Unit)
    def Nothing(implicit doc: Symtab) = fromSymbol(symbols.Nothing)
    def Any(implicit doc: Symtab) = fromSymbol(symbols.Any)
    def Throwable(implicit doc: Symtab) = fromSymbol(symbols.Throwable)

    def ZIO(implicit doc: Symtab) = fromSymbol(symbols.ZIO)
    def ZIOZStream(implicit doc: Symtab) = fromSymbol(symbols.ZIOZStream)
    def ZIOZSink(implicit doc: Symtab) = fromSymbol(symbols.ZIOZSink)
    def ZIOZManaged(implicit doc: Symtab) = fromSymbol(symbols.ZIOZManaged)

    def IzumiReflectTag(implicit doc: Symtab) =
      Type.Select(Term.Select(Term.Name("izumi"), Term.Name("reflect")), Type.Name("Tag"))

    def semanticTypeOf(tsymbol: Symbol)(implicit doc: Symtab): SemanticType = tsymbol.info
      .map {
        _.signature match {
          case t: ClassSignature => TypeRef(NoType, tsymbol, t.typeParameters.map(_.symbol).map(semanticTypeOf))
          case t @ TypeSignature(Nil, lowerBound, upperBound) if lowerBound == upperBound                  => lowerBound
          case t @ TypeSignature(Nil, lo @ TypeRef(_, symbols.Nothing, Nil), TypeRef(_, symbols.Any, Nil)) => lo
          case t @ TypeSignature(Nil, TypeRef(_, symbols.Nothing, Nil), hiBound)                           => hiBound
        }
      }
      .getOrElse(sys.error(s"No info for $tsymbol"))
  }

  def symbolOf(t: Tree with Stat.WithMods)(implicit doc: SemanticDocument) =
    Option(t.symbol)
      .filterNot(_.isNone)
      .orElse {
        //  Bug workaround:
        // If type has annotation then type symbol is stored in annotation symbol
        t.mods.filter(_.is[Mod.Annot]).map(_.symbol).headOption
      }

    def normalize(tpe: Type) = tpe
      .transform {
        case Type.Apply(Type.Name(name), types) if name.startsWith("Tuple") => Type.Tuple(types)
        case Type.With(l, r)                                                => Type.And(l, r)
        case tpe                                                            => tpe
      }
      .asInstanceOf[Type]

  def fromSemanticType(tpe: SemanticType)(implicit doc: Symtab): Type = normalize {
    tpe match {
      case TypeRef(prefix, symbol, typeArguments) =>
        val base = types.fromSymbol(symbol)
        if (typeArguments.isEmpty) base
        else Type.Apply(base, typeArguments.map(fromSemanticType))
      case WithType(types)            => types.map(fromSemanticType).reduce(Type.And(_, _))
      case SingleType(prefix, symbol) => Type.Singleton(types.termName(symbol))
      // Lossy conversions
      case ByNameType(tpe)                   => fromSemanticType(tpe)
      case StructuralType(tpe, declarations) => Type.Refine(Some(fromSemanticType(tpe)), stats = Nil)
      case AnnotatedType(annotations, tpe)   => fromSemanticType(tpe)
    }
  }

  def dealiasSymbol(symbol: Symbol)(implicit doc: Symtab): Symbol =
    symbol.info.getOrElse(sys.error(s"No info for ${symbol}")).signature match {
      case TypeSignature(tparams, lowerBound @ TypeRef(_, dealiased, _), upperBound) if lowerBound == upperBound =>
        dealiased
      case _ =>
        symbol
    }
  def tryDealias(tpe: SemanticType)(implicit doc: Symtab): SemanticType = tpe match {
    case TypeRef(prefix, symbol, typeArguments) =>
      val dealiasedSymbol = dealiasSymbol(symbol)

      def Any = types.semanticTypeOf(symbols.Any)
      def Throwable = types.semanticTypeOf(symbols.Throwable)
      def Nothing = types.semanticTypeOf(symbols.Nothing)

      (symbol, dealiasedSymbol, typeArguments) match {
        case (symbols.ZIOIO, symbols.ZIO, List(e, a))   => TypeRef(prefix, symbols.ZIO, List(Any, e, a))
        case (symbols.ZIOTask, symbols.ZIO, List(a))    => TypeRef(prefix, symbols.ZIO, List(Any, Throwable, a))
        case (symbols.ZIORIO, symbols.ZIO, List(r, a))  => TypeRef(prefix, symbols.ZIO, List(r, Throwable, a))
        case (symbols.ZIOUIO, symbols.ZIO, List(a))     => TypeRef(prefix, symbols.ZIO, List(Any, Nothing, a))
        case (symbols.ZIOURIO, symbols.ZIO, List(r, a)) => TypeRef(prefix, symbols.ZIO, List(r, Nothing, a))

        case (symbols.ZIOManaged, symbols.ZIOZManaged, List(e, a))   => TypeRef(prefix, symbols.ZIOZManaged, List(Any, e, a))
        case (symbols.ZIOTaskManaged, symbols.ZIOZManaged, List(a))  => TypeRef(prefix, symbols.ZIOZManaged, List(Any, Throwable, a))
        case (symbols.ZIORManaged, symbols.ZIOZManaged, List(r, a))  => TypeRef(prefix, symbols.ZIOZManaged, List(r, Throwable, a))
        case (symbols.ZIOUManaged, symbols.ZIOZManaged, List(a))     => TypeRef(prefix, symbols.ZIOZManaged, List(Any, Nothing, a))
        case (symbols.ZIOURManaged, symbols.ZIOZManaged, List(r, a)) => TypeRef(prefix, symbols.ZIOZManaged, List(r, Nothing, a))

        case (symbols.ZIOSink, symbols.ZIOZSink, List(outErr, in, l, z)) => TypeRef(prefix, symbols.ZIOZSink, List(Any, outErr, in, l, z))
        case (symbols.ZIOStream, symbols.ZIOZStream, List(e, a))         => TypeRef(prefix, symbols.ZIOZStream, List(Any, e, a))
        case (symbols.ZIOUStream, symbols.ZIOZStream, List(a))           => TypeRef(prefix, symbols.ZIOZStream, List(Any, Nothing, a))
        case _                                                           => tpe
      }
    case _ => tpe
  }

  sealed trait ImportedSymbol { def symbol: Symbol }
  object ImportedSymbol {
    case class Wildcard(symbol: Symbol) extends ImportedSymbol
    case class Name(symbol: Symbol) extends ImportedSymbol
  }

  def cleanupRules(pkgSymbols: Seq[Symbol], topLevelImportedSymbol: Seq[ImportedSymbol])(implicit
      doc: SemanticDocument
  ): List[String => String] =
    List[String => String](
      _.replaceAll(",(\\w)", ", $1"),
      _.replace(".`package`", ""),
      _.replace("scala.Predef.", ""),
      _.replaceAll(raw"scala\.(\w+)$$", "$1"),
      _.replaceAll(raw"scala\.(\w+)([\[\]\)\,])", "$1$2"),
      _.replace("java.lang.", ""),
      _.replace("zio.VersionSpecific.", "zio.")
    ) ++ (pkgSymbols.map(ImportedSymbol.Wildcard(_)) ++ topLevelImportedSymbol)
      .sortBy(_.symbol.value)
      .reverse
      .map { importedSymbol =>
        val symbol = importedSymbol.symbol
        val fqcn = types.termName(symbol).syntax
        importedSymbol match {
          case _: ImportedSymbol.Wildcard => (s: String) => s.replace(s"$fqcn.", "")
          case _: ImportedSymbol.Name     => (s: String) => s.replace(s"$fqcn.", symbol.displayName)
        }
      }

  def cleanupSyntax(pkgSymbols: Seq[Symbol], topLevelImportedSymbol: Seq[ImportedSymbol])(body: String)(implicit doc: SemanticDocument) =
    cleanupRules(pkgSymbols, topLevelImportedSymbol)
      .foldLeft(body) { case (acc, rule) => rule(acc) }

}
