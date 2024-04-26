/*
rule = ZIOAccessibleCodeGen
 */
package rewrite.accessible

import zio._
import zio.managed._
import zio.stream._
import zio.macros.accessible

@accessible
object AllCapabilities {
  trait Service {
    val static: UIO[String]
    def zeroArgs: UIO[Int]
    def zeroArgsWithParens(): UIO[Long]
    def singleArg(arg1: Int): UIO[String]
    def multiArgs(arg1: Int, arg2: Long): UIO[String]
    def multiParamLists(arg1: Int)(arg2: Long): UIO[String]
    def typedVarargs[T](arg1: Int, arg2: T*): UIO[T]
    def command(arg1: Int): UIO[Unit]
    def overloaded(arg1: Int): UIO[String]
    def overloaded(arg1: Long): UIO[String]

    val staticManaged: UManaged[String]
    def zeroArgsManaged: UManaged[Int]
    def zeroArgsTypedManaged[T]: UManaged[T]
    def zeroArgsWithParensManaged(): UManaged[Long]
    def singleArgManaged(arg1: Int): UManaged[String]
    def multiArgsManaged(arg1: Int, arg2: Long): UManaged[String]
    def multiParamListsManaged(arg1: Int)(arg2: Long): UManaged[String]
    def typedVarargsManaged[T](arg1: Int, arg2: T*): UManaged[T]
    def commandManaged(arg1: Int): UManaged[Unit]
    def overloadedManaged(arg1: Int): UManaged[String]
    def overloadedManaged(arg1: Long): UManaged[String]

    def function(arg1: Int): String
    def sink(arg1: Int): ZSink[Any, Nothing, Int, Int, List[Int]]
    def stream(arg1: Int): ZStream[Any, Nothing, Int]
  }
}

object AllCapabilitiesTest {
  val static: ZIO[AllCapabilities.Service, Nothing, String] = AllCapabilities.static
  def zeroArgs: ZIO[AllCapabilities.Service, Nothing, Int] = AllCapabilities.zeroArgs
  def zeroArgsWithParens(): ZIO[AllCapabilities.Service, Nothing, Long] = AllCapabilities.zeroArgsWithParens()
  def singleArg(arg1: Int): ZIO[AllCapabilities.Service, Nothing, String] = AllCapabilities.singleArg(arg1)
  def multiArgs(arg1: Int, arg2: Long): ZIO[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.multiArgs(arg1, arg2)
  def multiParamLists(arg1: Int)(arg2: Long): ZIO[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.multiParamLists(arg1)(arg2)
  def typedVarargs[T](arg1: Int, arg2: T*): ZIO[AllCapabilities.Service, Nothing, T] =
    AllCapabilities.typedVarargs[T](arg1, arg2: _*)
  def command(arg1: Int): ZIO[AllCapabilities.Service, Nothing, Unit] = AllCapabilities.command(arg1)
  def overloaded(arg1: Int): ZIO[AllCapabilities.Service, Nothing, String] = AllCapabilities.overloaded(arg1)
  def overloaded(arg1: Long): ZIO[AllCapabilities.Service, Nothing, String] = AllCapabilities.overloaded(arg1)

  val staticManaged: ZManaged[AllCapabilities.Service, Nothing, String] = AllCapabilities.staticManaged
  def zeroArgsManaged: ZManaged[AllCapabilities.Service, Nothing, Int] = AllCapabilities.zeroArgsManaged
  def zeroArgsTypedManaged[T]: ZManaged[AllCapabilities.Service, Nothing, T] = AllCapabilities.zeroArgsTypedManaged[T]
  def zeroArgsWithParensManaged(): ZManaged[AllCapabilities.Service, Nothing, Long] =
    AllCapabilities.zeroArgsWithParensManaged()
  def singleArgManaged(arg1: Int): ZManaged[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.singleArgManaged(arg1)
  def multiArgsManaged(arg1: Int, arg2: Long): ZManaged[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.multiArgsManaged(arg1, arg2)
  def multiParamListsManaged(arg1: Int)(arg2: Long): ZManaged[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.multiParamListsManaged(arg1)(arg2)
  def typedVarargsManaged[T](arg1: Int, arg2: T*): ZManaged[AllCapabilities.Service, Nothing, T] =
    AllCapabilities.typedVarargsManaged[T](arg1, arg2: _*)
  def commandManaged(arg1: Int): ZManaged[AllCapabilities.Service, Nothing, Unit] = AllCapabilities.commandManaged(arg1)
  def overloadedManaged(arg1: Int): ZManaged[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.overloadedManaged(arg1)
  def overloadedManaged(arg1: Long): ZManaged[AllCapabilities.Service, Nothing, String] =
    AllCapabilities.overloadedManaged(arg1)

  def function(arg1: Int): ZIO[AllCapabilities.Service, Throwable, String] = AllCapabilities.function(arg1)
  def sink(arg1: Int): ZSink[AllCapabilities.Service, Nothing, Int, Int, List[Int]] = AllCapabilities.sink(arg1)
  def stream(arg1: Int): ZStream[AllCapabilities.Service, Nothing, Int] = AllCapabilities.stream(arg1)
}
