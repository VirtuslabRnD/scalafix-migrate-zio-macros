package org.virtuslab.rewrites

import scalafix.v1._
import scala.meta._
import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala.Names.TermName
import scala.meta.Defn.Def
import scala.meta.internal.trees.Fresh

class ZIOMockableCodeGen extends SemanticRule("ZIOMockableCodeGen") with ZIOCodeGen {
  override def fix(implicit doc: SemanticDocument): Patch = {
    object names {
      object MockableAnnotation extends NameExtractor(symbols.MockableAnnotation, shouldBeRemoved = true)

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
          MockableAnnotation
          // format: on
        )
        def unapply(t: Name): Option[NameExtractor] =
          extractors.iterator.collectFirst {
            case extractor if extractor.unapply(t) => extractor
          }
      }
    }

    implicit val patches: PatchesBuilder = Seq.newBuilder[Patch]

    val (companionObjects, typeSymbols, pkgSymbols, importedSymbols) = {
      val companionObjects = Map.newBuilder[String, Defn.Object]
      val typeSymbols = mutable.Map.empty[String, Symbol]
      val pkgSymbols = List.newBuilder[Symbol]
      val importedSymbols = Set.newBuilder[ImportedSymbol]
      doc.tree.traverse {
        case t @ Importee.Name(names.NameExtractor(name)) if name.shouldBeRemoved =>
          patches += Patch.removeImportee(t)
        case t @ Importee.Rename(names.NameExtractor(name), alias) =>
          if (name.shouldBeRemoved) patches += Patch.removeImportee(t)
          name.aliases += alias.value

        case t: Defn.Object => companionObjects += t.name.value -> t
        // SemanticDB does not always set symbols for Type.Names, e.g. ther'e missing in annotation type parameters
        case t: Type =>
          // Yes, the scalameta Types equality is not based on the the product values, the most reliable seems to be depending on structure or type rendering
          t.symbol.asNonEmpty.filter(_.info.exists(t => t.isClass || t.isTrait || t.isType)).foreach { symbol =>
            t match {
              case t: Type.Name => if (t.value != "Any") typeSymbols.getOrElseUpdate(t.value, symbol)
              case t: Type.Select =>
                typeSymbols.getOrElseUpdate(t.name.value, symbol)
                typeSymbols.getOrElseUpdate(t.toString(), symbol)
              case _ => ()
            }
          }
        case t @ (_: Importee.Name | _: Importee.Rename) if t.symbol.asNonEmpty.isDefined =>
          val clsSymbol = {
            val symValue = t.symbol.value
            if (symValue.endsWith("#")) Some(t.symbol)
            else if (symValue.endsWith(".") && t.symbol.info.exists(_.isObject)) {
              val clsSymbol = Symbol(symValue.stripSuffix(".") + "#")
              Option(clsSymbol).filter(_.info.isDefined)
            } else None
          }
          clsSymbol.foreach { symbol =>
            val name = t match {
              case Importee.Name(name) =>
                typeSymbols.getOrElseUpdate(name.value, symbol)
              case Importee.Rename(name, alias) =>
                typeSymbols.getOrElseUpdate(name.value, symbol)
                typeSymbols.getOrElseUpdate(alias.value, symbol)
            }
          }
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
      (companionObjects.result(), typeSymbols.toMap, pkgSymbols.result(), importedSymbols.result().toList)
    }

    // All this workarounds are needed becouse types might have missing/malformed symbols (mostly in annotaitons)
    def symbolFromType(tpe: Type): Option[Symbol] = tpe match {
      case t: Type.Name =>
        def validate(sym: Symbol) = sym.displayName == t.value
        typeSymbols
          .get(t.value)
          .filter(validate)
          .orElse(
            pkgSymbols
              .map(pkg => Symbol(s"${pkg}${t.value}#"))
              .find(_.info.exists(t => t.isType || t.isClass || t.isTrait))
          )
          .orElse(tpe.symbol.asNonEmpty.filterNot(_.isLocal).filter(validate))
      case t: Type.Select =>
        typeSymbols
          .get(t.toString())
          .orElse(typeSymbols.get(t.name.value))
          .orElse(pkgSymbols.map(pkg => Symbol(s"${pkg}${t.name.value}#")).find(_.info.isDefined))
          .orElse(tpe.symbol.asNonEmpty.filterNot(_.isLocal))
      case _ => None
    }

    def isTagged(param: Param)(implicit doc: SemanticDocument): Boolean = {
      val fromSymbol =
        param.tpe.symbol.asNonEmpty
          .orElse(symbolFromType(param.tpe))
          .flatMap(_.info)
          .map(_.signature)
          .exists {
            case ClassSignature(_, parents, _, _) =>
              parents.exists {
                case TypeRef(_, symbols.IzumiReflectTag, _) => true
                case _                                      => false
              }
            case _ => false
          }
      def fromType = param.tpe.collect {
        case Type.Name("EnvironmentTag")                                                                                      => true
        case Type.Select(Term.Select(Term.Name("zio"), _), Type.Name(name)) if name.startsWith("Tag") || name.endsWith("Tag") => true
      }.nonEmpty

      fromSymbol || fromType
    }

    doc.tree.traverse { case annotatedModule: Defn.Object =>
      val accessors = annotatedModule.mods
        .collectFirst {
          case annot @ Mod.Annot(
                Init(Type.Apply(names.MockableAnnotation(), mockedService :: Nil), _, Nil)
              ) =>
            patches += Patch.removeTokens(annot.tokens)

            val methodInfos = for {
              serviceSymbol <- symbolFromType(mockedService).toList
              dealiasedServiceSymbol = dealiasSymbol(serviceSymbol)
              _ = println(s"Creating Mock[${mockedService}] - symbol: ${dealiasedServiceSymbol.value}")
              info <- dealiasedServiceSymbol.info.toList
              case ClassSignature(clsTParams, _, _, decls) <- info.signature match {
                case cls: ClassSignature => List(cls)
                case _                   => Nil
              }
              decl <- decls
              case methodSig @ MethodSignature(methodTParams, methodParams, methodRType) <- decl.signature match {
                case sig: MethodSignature => List(sig)
                case _                    => Nil
              }
            } yield {
              val params = methodParams.flatten
                .map { param =>
                  val ValueSignature(paramTpe) = param.signature: @unchecked
                  Param(param.displayName, fromSemanticType(paramTpe))
                }
                .filterNot(isTagged)
              val typeParams = methodTParams
                .map { tParam =>
                  Type.Name(tParam.symbol.displayName)
                }

              val capability = tryDealias(methodRType) match {
                case t @ TypeRef(_, symbols.ZIO, typeParams) =>
                  val List(r, e, a) = typeParams.map(fromSemanticType): @unchecked
                  Capability.Effect(r, e, a)

                case TypeRef(_, symbols.ZIOZStream, typeParams) =>
                  val List(r, e, a) = typeParams.map(fromSemanticType): @unchecked
                  Capability.Stream(r, e, a)

                case TypeRef(_, symbols.ZIOZSink, typeParams) =>
                  val List(r, e, a0, a, b) = typeParams.map(fromSemanticType): @unchecked
                  Capability.Sink(r, e, a0, a, b)

                case rType => Capability.Method(fromSemanticType(rType))
              }

              val interface = params.map(_.tpe) match {
                case Nil        => types.Unit
                case tpe :: Nil => tpe
                case types      => Type.Tuple(types)
              }

              MethodInfo(decl.symbol, capability, params, typeParams, interface)
            }
            val declarationOrder = methodInfos.map(_.symbol.displayName).zipWithIndex.toMap
            val methods =
              methodInfos
                .groupBy(_.symbol.displayName)
                .toList
                .sortBy { case (name, _) => declarationOrder(name) }

            if (methods.isEmpty) {
              case class NoMethodsFound() extends Diagnostic {
                override def position = annotatedModule.pos
                override def message: String =
                  s"Cannot create diagnostics, have not found any methods in ${mockedService}" +
                    s"""
                  |tpe=${mockedService.structureLabeled}
                  |typeSymol=${typeSymbols.get(mockedService.toString())}
                  |symbol=${mockedService.symbol} / ${symbolFromType(mockedService)}
                  |dealiased=${symbolFromType(mockedService).map(dealiasSymbol)}
                  |info=${symbolFromType(mockedService).map(dealiasSymbol).flatMap(_.info)}
                  |sig=${symbolFromType(mockedService)
                        .map(dealiasSymbol)
                        .flatMap(_.info)
                        .map(v => v -> v.signature.structureLabeled)}
                  |typeSymbols: ${typeSymbols.toList.sortBy(_._1).mkString("\n - ")}
                  |""".stripMargin
              }
              patches += Patch.lint(NoMethodsFound())
            }

            def sortOverloads(seq: List[MethodInfo]): List[MethodInfo] = {
              import scala.math.Ordering.Implicits._
              implicit val paramOrder: Ordering[Param] = Ordering.by(v => (v.name, v.tpe.toString()))
              seq.sortBy(_.params)
            }
            val tags = methods.map {
              case (name, info :: Nil) => makeTag(name, info)
              case (name, overloads) =>
                val tagName = name.capitalize
                val overloadedTags = sortOverloads(overloads).zipWithIndex.map { case (info, idx) =>
                  makeTag(s"_$idx", info)
                }
                val ident = "  "
                s"""object $tagName {
                |${ident}  ${overloadedTags.mkString(s"\n$ident  ")}
                |${ident}}""".stripMargin
            }
            val mocks = methods.flatMap {
              case (name, info :: Nil) =>
                makeMock(name, info, overloadIndex = None) :: Nil
              case (name, overloads) =>
                sortOverloads(overloads).zipWithIndex.map { case (info, idx) =>
                  makeMock(name, info, Some(s"_$idx"))
                }
            }

            val parents = annotatedModule.templ.inits match {
              case Nil   => ""
              case inits => inits.map(v => s"with $v").mkString(" ") + " "
            }
            val body = annotatedModule.templ.stats match {
              case Nil => ""
              case stats =>
                val ident = "  "
                ident + stats.mkString(s"\n$ident") + "\n"
            }
            val generatedObject = cleanupSyntax(pkgSymbols, importedSymbols) {
              s"""
              |object ${annotatedModule.name.value} extends Mock[$mockedService] $parents{
              |$body  // format: off
              |  // Generated by ZIO Mockable CodeGen Scalafix Rule
              |  ${tags.mkString("\n  ")}
              |  override val compose: zio.URLayer[zio.mock.Proxy, $mockedService] = zio.ZLayer.fromZIO[zio.mock.Proxy, Nothing, $mockedService](
              |    zio.ZIO.service[zio.mock.Proxy].flatMap { proxy =>
              |      withRuntime[zio.mock.Proxy, $mockedService] { rt =>
              |        class MockImpl extends $mockedService {
              |          ${mocks.mkString("\n          ")}
              |        }
              |        zio.ZIO.succeed(new MockImpl())
              |  }})
              |  // format: on
              |}""".stripMargin
            }
            patches += Patch.replaceTree(annotatedModule, generatedObject).atomic
            patches += Patch.addGlobalImport(symbols.ZIOMock)
            ()
        }
    }

    patches.result().asPatch
  }

  def makeTag(name: String, info: MethodInfo)(implicit patches: PatchesBuilder, doc: SemanticDocument) = {
    val tagName = name.capitalize
    val (i, e, a) = (info.i, info.e, info.a)

    val parent = info.capability match {
      case _: Capability.Effect =>
        (info.polyI, info.polyE, info.polyA) match {
          case (false, false, false) => s"Effect[$i, $e, $a]"
          case (true, false, false)  => s"Poly.Effect.Input[$e, $a]"
          case (false, true, false)  => s"Poly.Effect.Error[$i, $a]"
          case (false, false, true)  => s"Poly.Effect.Output[$i, $e]"
          case (true, true, false)   => s"Poly.Effect.InputError[$a]"
          case (true, false, true)   => s"Poly.Effect.InputOutput[$e]"
          case (false, true, true)   => s"Poly.Effect.ErrorOutput[$i]"
          case (true, true, true)    => s"Poly.Effect.InputErrorOutput"
        }
      case _: Capability.Stream =>
        (info.polyI, info.polyE, info.polyA) match {
          case (false, false, false) => s"Stream[$i, $e, $a]"
        }
      case _: Capability.Method =>
        (info.polyI, info.polyE, info.polyA) match {
          case (false, false, false) => s"Method[$i, $e, $a]"
          case (true, false, false)  => s"Poly.Method.Input[$e, $a]"
          case (false, true, false)  => s"Poly.Method.Error[$i, $a]"
          case (false, false, true)  => s"Poly.Method.Output[$i, $e]"
          case (true, true, false)   => s"Poly.Method.InputError[$a]"
          case (true, false, true)   => s"Poly.Method.InputOutput[$e]"
          case (false, true, true)   => s"Poly.Method.ErrorOutput[$i]"
          case (true, true, true)    => "Poly.Method.InputErrorOutput"
        }

    }
    s"case object ${tagName} extends $parent"
  }

  def makeMock(name: String, info: MethodInfo, overloadIndex: Option[String])(implicit
      doc: SemanticDocument
  ) = {
    val tagName = name.capitalize
    val (r, i, e, a) = (info.r, info.i, info.e, info.a)

    // val typeParamArgs = info.symbol.typeParams.map(s => toTypeDef(s))
    val tag = (info.polyI, info.polyE, info.polyA, overloadIndex) match {
      case (false, false, false, None)        => s"$tagName"
      case (true, false, false, None)         => s"$tagName.of[$i]"
      case (false, true, false, None)         => s"$tagName.of[$e]"
      case (false, false, true, None)         => s"$tagName.of[$a]"
      case (true, true, false, None)          => s"$tagName.of[$i, $e]"
      case (true, false, true, None)          => s"$tagName.of[$i, $a]"
      case (false, true, true, None)          => s"$tagName.of[$e, $a]"
      case (true, true, true, None)           => s"$tagName.of[$i, $e, $a]"
      case (false, false, false, Some(index)) => s"$tagName.$index"
      case (true, false, false, Some(index))  => s"$tagName.$index.of[$i]"
      case (false, true, false, Some(index))  => s"$tagName.$index.of[$e]"
      case (false, false, true, Some(index))  => s"$tagName.$index.of[$a]"
      case (true, true, false, Some(index))   => s"$tagName.$index.of[$i, $e]"
      case (true, false, true, Some(index))   => s"$tagName.$index.of[$i, $a]"
      case (false, true, true, Some(index))   => s"$tagName.$index.of[$e, $a]"
      case (true, true, true, Some(index))    => s"$tagName.$index.of[$i, $e, $a]"
    }

    val symbolInfo = info.symbol.info
    val mods = "final override"

    // TODO: used only for val
    val returnType = info.capability match {
      case Capability.Method(t)       => t
      case Capability.Stream(r, e, a) => Type.Apply(types.ZIOZStream, List(r, e, a))
      case _                          => Type.Apply(types.ZIO, List(r, e, a))
    }

    def wrapInUnsafe(tree: String) = s"""zio.Unsafe.unsafe { case given zio.Unsafe => $tree }"""

    val returnValue = {
      val params = info.params.map(_.name)
      def proxyArgs = params match {
        case Nil    => tag
        case params => s"$tag, ${params.mkString(", ")}"
      }
      info.capability match {
        case _: Capability.Effect => s"proxy($proxyArgs)"
        case _: Capability.Method => wrapInUnsafe(s"rt.unsafe.run(proxy($proxyArgs)).getOrThrow()")
        case _: Capability.Stream => wrapInUnsafe(s"rt.unsafe.run(proxy($proxyArgs)).getOrThrowFiberFailure()")
        case _: Capability.Sink =>
          wrapInUnsafe(
            s"rt.unsafe.run(proxy($proxyArgs).catchAll(error => zio.UIO(zio.stream.ZSink.fail(error)))).getOrThrowFiberFailure()"
          )
      }
    }
    if (symbolInfo.exists(_.isVal)) s"$mods val $name: $returnType = $returnValue"
    else {
      symbolInfo
        .map(_.signature)
        .collectFirst { case MethodSignature(typeParametersInfo, parameterLists, _) =>
          val tParams = typeParametersInfo.zip(info.typeParams).map { case (tParamInfo, tParam) =>
            s"${tParam}${tParamInfo.signature}"
          }
          val paramClouses = parameterLists.map { params =>
            Term.ParamClause(
              values = params.map { ts =>
                val name = ts.displayName
                val tpe = ts.signature match {
                  case ValueSignature(tpe) => fromSemanticType(tpe)
                }
                val isImplicit = ts.isImplicit
                Term.Param(
                  mods = if (isImplicit) List(Mod.Using()) else Nil,
                  name = if (isImplicit) Name.Anonymous() else Term.Name(name),
                  decltpe = Some(tpe),
                  default = None
                )
              },
              mod = Option(Mod.Using()).filter(_ => params.exists(_.isImplicit))
            )
          }
          val typeParams = if (tParams.isEmpty) "" else tParams.mkString("[", ",", "]")
          s"$mods def $name$typeParams${paramClouses.mkString}: $returnType = $returnValue"
        }
        .get
    }
  }

  sealed trait Capability
  object Capability {
    case class Method(a: Type) extends Capability
    case class Effect(r: Type, e: Type, a: Type) extends Capability
    case class Stream(r: Type, e: Type, a: Type) extends Capability
    case class Sink(r: Type, e: Type, a0: Type, a: Type, b: Type) extends Capability
  }

  case class Param(name: String, tpe: Type) {}
  case class MethodInfo(
      symbol: Symbol,
      capability: Capability,
      params: List[Param],
      typeParams: List[Type],
      i: Type
  )(implicit doc: SemanticDocument) {

    val r: Type = normalize {
      capability match {
        case Capability.Effect(r, _, _)     => r
        case Capability.Sink(r, _, _, _, _) => r
        case Capability.Stream(r, _, _)     => r
        case Capability.Method(_)           => types.Any
      }
    }

    val e: Type = normalize {
      capability match {
        case Capability.Effect(_, e, _)     => e
        case Capability.Sink(_, e, _, _, _) => e
        case Capability.Stream(_, e, _)     => e
        case Capability.Method(_)           => types.Throwable
      }
    }

    val a: Type = normalize {
      capability match {
        case Capability.Effect(_, _, a)      => a
        case Capability.Sink(_, e, a0, a, b) => Type.Apply(types.ZIOZSink, List(types.Any, e, a0, a, b))
        case Capability.Stream(_, _, a)      => a
        case Capability.Method(a)            => a
      }
    }

    def containsType(tpe: Type, expected: Type): Boolean = {
      (tpe, expected) match {
        case (Type.Name(n1), Type.Name(n2)) => n1 == n2
        case (Type.Select(_, tpe), _)       => containsType(tpe, expected)
        case (Type.Apply(_, tps), _)        => tps.exists(containsType(_, expected))
        case (Type.Tuple(tps), _)           => tps.exists(containsType(_, expected))
      }
    }
    val polyI: Boolean = typeParams.exists(containsType(i, _))
    val polyE: Boolean = typeParams.exists(containsType(e, _))
    val polyA: Boolean = typeParams.exists(containsType(a, _))
  }

}
