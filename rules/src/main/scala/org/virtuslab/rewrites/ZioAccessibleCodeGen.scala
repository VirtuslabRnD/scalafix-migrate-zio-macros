package org.virtuslab.rewrites

import scalafix.v1._
import scala.meta._
import scala.collection.mutable
import scala.meta.Tree.WithTParamClause
import scala.meta.Tree.WithParamClauseGroups
import scala.meta.Member.ParamClauseGroup
import scala.meta.Stat.WithMods
import scala.meta.Stat.WithTemplate

class ZIOAccessibleCodeGen extends SemanticRule("ZIOAccessibleCodeGen") with ZIOCodeGen {

  override def fix(implicit doc: SemanticDocument): Patch = {
    object names {
      object AccessibleAnnotation extends NameExtractor(symbols.AccessibleAnnotation, shouldBeRemoved = true)

      val Constructor = Term.Name("<init>")

      sealed abstract class NameExtractor(symbol: Symbol, val shouldBeRemoved: Boolean = false) {
        val name: String = symbol.displayName
        val aliases: mutable.Set[String] = mutable.Set.empty
        def aliasOrName: String = aliases.headOption.getOrElse(name)
        def unapply(t: Name): Boolean = {
          t.value == name || aliases.contains(t.value)
        }
      }
      object NameExtractor {
        private lazy val extractors = Seq[NameExtractor](
          // format: off
          AccessibleAnnotation
          // format: on
        )
        def unapply(t: Name): Option[NameExtractor] =
          extractors.iterator.collectFirst {
            case extractor if extractor.unapply(t) => extractor
          }
      }
    }

    val patches = Seq.newBuilder[Patch]

    val (companionObjects, pkgSymbols, topLevelImportedSymbol) = {
      val companionObjects = Map.newBuilder[String, Defn.Object]
      val pkgSymbols = List.newBuilder[Symbol]
      val importedSymbols = Set.newBuilder[ImportedSymbol]
      doc.tree.traverse {
        case t @ Importee.Name(names.NameExtractor(name)) if name.shouldBeRemoved =>
          patches += Patch.removeImportee(t)
        case t @ Importee.Rename(names.NameExtractor(name), alias) =>
          if (name.shouldBeRemoved) patches += Patch.removeImportee(t)
          name.aliases += alias.value
        case t: Defn.Object => companionObjects += t.name.value -> t
        case Pkg(ref, stats) =>
          pkgSymbols += ref.symbol
          importedSymbols ++= stats
            .collect { case t: Import =>
              t.importers.flatMap { importer =>
                importer.importees.collect {
                  case Importee.Wildcard() => ImportedSymbol.Wildcard(importer.ref.symbol)
                  case Importee.Name(name) => ImportedSymbol.Name(name.symbol)
                }
              }
            }
            .flatten
            .filter(_.symbol.info.isDefined)
      }
      (companionObjects.result(), pkgSymbols.result(), importedSymbols.result().toList)
    }

    case class ModuleInfo(
        module: Option[Defn.Object],
        service: Tree with Member.Type with WithMods with WithTemplate,
        serviceTypeParams: List[Type.Param]
    )

    def accessibleAnnotation(t: Stat.WithMods): Option[Mod.Annot] = t.mods.collectFirst {
      case annot @ Mod.Annot(Init(names.AccessibleAnnotation(), _, _)) => annot
    }
    val generatedServices = mutable.Set.empty[Symbol]
    doc.tree.traverse {
      case defnTree: Stat.WithMods if accessibleAnnotation(defnTree).isDefined =>
        accessibleAnnotation(defnTree).map(_.tokens).foreach(patches += Patch.removeTokens(_))

        val moduleInfo = defnTree match {
          case tree: Defn.Trait =>
            Some(ModuleInfo(companionObjects.get(tree.name.value), tree, tree.tparamClause.values))
          case tree: Defn.Class =>
            Some(ModuleInfo(companionObjects.get(tree.name.value), tree, tree.tparamClause.values))
          case module: Defn.Object =>
            module.templ.stats.collectFirst {
              case service: Defn.Class => ModuleInfo(Some(module), service, service.tparamClause.values)
              case service: Defn.Trait => ModuleInfo(Some(module), service, service.tparamClause.values)
            }
        }

        for {
          moduleInfo <- moduleInfo
          serviceSymbol <- symbolOf(moduleInfo.service)
          if generatedServices.add(serviceSymbol)
          val serviceName = moduleInfo.service.name
        } {
          val accessors =
            moduleInfo.service.templ.stats
              .collect {
                case DefDef(tree) if tree.name != names.Constructor =>
                  makeAccessor(
                    tree.mods.filter {
                      case Mod.Annot(Init(_, name, _)) => !name.value.endsWith("deprecated")
                      case _                           => true
                    },
                    tree.name,
                    typeInfo(tree), // withThrowing(mods, tree),
                    moduleInfo.serviceTypeParams,
                    tree.paramClauseGroup,
                    isVal = false,
                    serviceName,
                    serviceSymbol
                  )

                case ValDef(tree) =>
                  makeAccessor(
                    Nil,
                    tree match {
                      case Defn.Val(_, List(Pat.Var(name)), _, _) => name
                      case Decl.Val(_, List(Pat.Var(name)), _)    => name
                    },
                    typeInfo(tree),
                    moduleInfo.serviceTypeParams,
                    None,
                    isVal = true,
                    serviceName,
                    serviceSymbol
                  )
              }

          if (accessors.nonEmpty) {
            println(s"Creating accessor of ${symbolOf(moduleInfo.service).get} in module: ${moduleInfo.module.flatMap(symbolOf)}")
            val genAccessors = cleanupSyntax(pkgSymbols = pkgSymbols, topLevelImportedSymbol = topLevelImportedSymbol) {
              s"""
              |  // format: off
              |  // Generated by ZIO Accessible CodeGen Scalafix Rule
              |${accessors.map(_.syntax).mkString("  ", "\n  ", "")}
              |  // format: on""".stripMargin
            }
            val genCompanionObject =
              s"""object ${serviceName.value} {
              |$genAccessors
              |}""".stripMargin
            patches += {
              moduleInfo.module match {
                case Some(module @ Defn.Object(_, _, t @ Template(_, _, self, stats))) =>
                  if (stats.nonEmpty)
                    Patch.addRight(
                      stats.lastOption.getOrElse(self),
                      s"\n$genAccessors"
                    )
                  else Patch.replaceTree(module, genCompanionObject)
                case _ => Patch.addRight(defnTree, "\n\n" + genCompanionObject)
              }
            }
          }
        }
    }

    patches.result().asPatch
  }

  def makeAccessor(
      mods: List[Mod],
      name: Term.Name,
      capability: Capability,
      serviceTypeParams: List[Type.Param],
      paramClauseGroup: Option[Member.ParamClauseGroup],
      isVal: Boolean,
      serviceName: Type.Name,
      serviceSymbol: Symbol
  )(implicit doc: Symtab): Tree = {
    val serviceType = types.fromSymbol(serviceSymbol)
    def withR(tpe: Type, r: Type): Type =
      if (r.structure != types.Any.structure) Type.And(tpe, r)
      else tpe

    val returnType = capability match {
      case Capabiltity.Effect(r, e, a)     => Type.Apply(types.ZIO, List(withR(serviceType, r), e, a))
      case Capabiltity.Managed(r, e, a)    => Type.Apply(types.ZIOZManaged, List(withR(serviceType, r), e, a))
      case Capabiltity.Stream(r, e, a)     => Type.Apply(types.ZIOZStream, List(withR(serviceType, r), e, a))
      case Capabiltity.Sink(r, e, a, l, b) => Type.Apply(types.ZIOZSink, List(withR(serviceType, r), e, a, l, b))
      case Capabiltity.Method(a)           => Type.Apply(types.ZIO, List(serviceType, types.Nothing, a))
      case Capabiltity.ThrowingMethod(a)   => Type.Apply(types.ZIO, List(serviceType, types.Throwable, a))
    }

    val typeArgs = paramClauseGroup.toList.flatMap(_.tparamClause)
    val paramLists = paramClauseGroup.toList.flatMap(_.paramClauses)
    def selector(lhs: Term = Term.Placeholder()) = {
      val methodSelect = Term.Select(lhs, name)
      val base =
        if (typeArgs.isEmpty) methodSelect
        else Term.ApplyType(methodSelect, typeArgs.map(v => Type.Name(v.name.value)))
      if (paramLists.isEmpty) base
      else
        paramLists
          .filterNot(_.mod.exists(_.isAny[Mod.Implicit, Mod.Using]))
          .map { paramms =>
            paramms.map { param =>
              val ident = Term.Name(param.name.value)
              val isRepeated = param.decltpe.exists(_.is[Type.Repeated])
              if (isRepeated) Term.Repeated(ident)
              else ident
            }
          }
          .foldLeft(base)(Term.Apply(_, _))
    }
    def select(fullyQualifiedName: String): Term = fullyQualifiedName.split('.').map(Term.Name(_)).toList match {
      case head :: Nil  => head
      case head :: tail => tail.foldLeft[Term](head)(Term.Select(_, _))
    }
    val returnValue: Term = capability match {
      case _: Capabiltity.Effect =>
        Term.Apply(
          Term.ApplyType(select("zio.ZIO.serviceWithZIO"), List(serviceName)),
          Term.AnonymousFunction(selector()) :: Nil
        )

      case _: Capabiltity.Managed =>
        Term.Apply(
          Term.ApplyType(select("zio.managed.ZManaged.serviceWithManaged"), List(serviceName)),
          Term.AnonymousFunction(selector()) :: Nil
        )

      case _: Capabiltity.Stream =>
        Term.Apply(
          Term.ApplyType(select("zio.stream.ZStream.serviceWithStream"), List(serviceName)),
          Term.AnonymousFunction(selector()) :: Nil
        )
      case Capabiltity.Sink(r, e, a, l, b) =>
        Term.Apply(
          Term.ApplyType(
            Term.ApplyType(select("zio.stream.ZSink.environmentWithSink"), List(serviceName)),
            List(Type.And(serviceType, r), e, a, l, b)
          ),
          Term.AnonymousFunction(
            selector(Term.ApplyType(Term.Select(Term.Placeholder(), Term.Name("get")), List(serviceType)))
          ) :: Nil
        )

      case _: Capabiltity.ThrowingMethod =>
        val paramName = Term.Name("s")
        Term.Apply(
          Term.ApplyType(select("zio.ZIO.serviceWithZIO"), List(serviceName)),
          Term.Function(
            Term.Param(Nil, paramName, None, None) :: Nil,
            Term.Apply(select("zio.ZIO.attempt"), selector(paramName) :: Nil)
          ) :: Nil
        )
      case _ =>
        Term.Apply(
          Term.ApplyType(select("zio.ZIO.serviceWith"), List(serviceName)),
          Term.AnonymousFunction(selector()) :: Nil
        )
    }
    if (isVal)
      Defn.Val(mods, List(Pat.Var(name)), Some(returnType), returnValue)
    else {
      val usingServiceTag = Term.ParamClause(
        mod = Some(Mod.Using()),
        values = Term.Param(
          Nil,
          Name.Anonymous(),
          decltpe = Some(Type.Apply(types.IzumiReflectTag, serviceType :: Nil)),
          None
        ) :: Nil
      )
      val paramClouses = paramClauseGroup match {
        case None => ParamClauseGroup(Type.ParamClause(Nil), usingServiceTag :: Nil)
        case Some(group @ ParamClauseGroup(_, paramss)) =>
          val adaptedParamss = paramss.map(paramss =>
            if (!paramss.mod.exists(_.is[Mod.Implicit])) paramss
            else paramss.copy(values = paramss.values.map(_.copy(name = Name.Anonymous())), mod = Some(Mod.Using()))
          )
          group.copy(paramClauses = adaptedParamss :+ usingServiceTag)
      }
      Defn.Def(mods, name, Some(paramClouses), Some(returnType), returnValue)
    }
  }

  def typeInfo(t: Tree)(implicit doc: SemanticDocument): Capability = {
    val tpe = t.symbol.info
      .map(_.signature)
      .collect {
        case ValueSignature(tpe)                                         => tryDealias(tpe)
        case MethodSignature(typeParameters, parameterLists, returnType) => tryDealias(returnType)
      }
      .get
    tpe match {
      case TypeRef(_, symbol, tParams) =>
        (dealiasSymbol(symbol), tParams.map(fromSemanticType)) match {
          case (symbols.ZIO, List(r, e, a))            => Capabiltity.Effect(r, e, a)
          case (symbols.ZIOZManaged, List(r, e, a))    => Capabiltity.Managed(r, e, a)
          case (symbols.ZIOZSink, List(r, e, a, l, b)) => Capabiltity.Sink(r, e, a, l, b)
          case (symbols.ZIOZStream, List(r, e, a))     => Capabiltity.Stream(r, e, a)
          case _                                       => Capabiltity.Method(fromSemanticType(tpe))
        }
      case _ => Capabiltity.Method(fromSemanticType(tpe))
    }
  }

  sealed trait Capability
  object Capabiltity {
    case class Effect(r: Type, e: Type, a: Type) extends Capability
    case class Managed(r: Type, e: Type, a: Type) extends Capability
    case class Method(a: Type) extends Capability
    case class Sink(r: Type, e: Type, a: Type, l: Type, b: Type) extends Capability
    case class Stream(r: Type, e: Type, a: Type) extends Capability
    case class ThrowingMethod(a: Type) extends Capability
  }

}
