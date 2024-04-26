/*
rule = ZIOAccessibleCodeGen
 */
package rewrite.accessible

import zio._
import zio.macros.{accessible => accessibleAlias}

trait Foo { val value: String }
case class Bar(value: String) extends Foo
case class Wrapped[T](value: T)

@accessibleAlias
trait Service {
  def get(key: String): UIO[Int]
  def set(key: String, value: Int): UIO[Unit]
  def reset: UIO[Unit]
  def io: IO[String, Long]
  def task: Task[Long]
  def uio: UIO[Long]
  def urio: URIO[String, Long]
  def poly1[A: EnvironmentTag](a: A): UIO[Unit]
  def poly2[A: EnvironmentTag]: IO[A, Unit]
  def poly3[A: EnvironmentTag]: UIO[A]
  def poly4[A: EnvironmentTag, B: EnvironmentTag](a: A): IO[B, Unit]
  def poly5[A: EnvironmentTag, B: EnvironmentTag](a: A): IO[Unit, B]
  def poly6[A: EnvironmentTag, B: EnvironmentTag]: IO[A, B]
  def poly7[A: EnvironmentTag, B: EnvironmentTag, C: EnvironmentTag](a: A): IO[B, C]
  def poly8[A: EnvironmentTag]: UIO[(A, String)]
  def poly9[A <: Foo: EnvironmentTag]: UIO[A]
  def poly10[A: EnvironmentTag](a: Wrapped[A]): UIO[A]
}

object TestBasic {
  val get: ZIO[Service with Any, Nothing, Int] = Service.get("foo")
  val set: ZIO[Service with Any, Nothing, Unit] = Service.set("foo", 42)
  val reset: ZIO[Service with Any, Nothing, Unit] = Service.reset
  val io: ZIO[Service with Any, Throwable, Long] = Service.task
  val uio: ZIO[Service with Any, Nothing, Long] = Service.uio
  val urio: ZIO[Service with String, Nothing, Long] = Service.urio
  val poly1: ZIO[Service with Any, Nothing, Unit] = Service.poly1("foo")
  val poly2: ZIO[Service with Any, String, Unit] = Service.poly2[String]
  val poly3: ZIO[Service with Any, Nothing, String] = Service.poly3[String]
  val poly4: ZIO[Service with Any, Long, Unit] = Service.poly4[Int, Long](42)
  val poly5: ZIO[Service with Any, Unit, Long] = Service.poly5[Int, Long](42)
  val poly6: ZIO[Service with Any, String, Int] = Service.poly6[String, Int]
  val poly7: ZIO[Service with Any, Long, Int] = Service.poly7[String, Long, Int]("foo")
  val poly8: ZIO[Service with Any, Nothing, (Int, String)] = Service.poly8[Int]
  val poly9: ZIO[Service with Any, Nothing, Bar] = Service.poly9[Bar]
  val poly10: ZIO[Service with Any, Nothing, String] = Service.poly10(Wrapped("string"))
}
