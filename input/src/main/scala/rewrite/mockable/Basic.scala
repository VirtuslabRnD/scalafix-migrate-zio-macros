/*
rule = ZIOMockableCodeGen
 */
package rewrite.mockable

import zio._
import zio.mock.{mockable, Mock}

trait Foo { val value: String }
case class Bar(value: String) extends Foo
case class Wrapped[T](value: T)

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

trait ServiceCompanion
@mockable[Service]
object ServiceMock extends ServiceCompanion {
  val foo = 42
}

object TestBasic {
  implicitly[ServiceMock.type <:< Mock[Service]]
  implicitly[ServiceMock.Get.type <:< ServiceMock.Effect[String, Nothing, Int]]
  implicitly[ServiceMock.Set.type <:< ServiceMock.Effect[(String, Int), Nothing, Unit]]
  implicitly[ServiceMock.Reset.type <:< ServiceMock.Effect[Unit, Nothing, Unit]]
  implicitly[ServiceMock.Io.type <:< ServiceMock.Effect[Unit, String, Long]]
  implicitly[ServiceMock.Task.type <:< ServiceMock.Effect[Unit, Throwable, Long]]
  implicitly[ServiceMock.Uio.type <:< ServiceMock.Effect[Unit, Nothing, Long]]
  implicitly[ServiceMock.Urio.type <:< ServiceMock.Effect[Unit, Nothing, Long]]
  implicitly[ServiceMock.Poly1.type <:< ServiceMock.Poly.Effect.Input[Nothing, Unit]]
  implicitly[ServiceMock.Poly2.type <:< ServiceMock.Poly.Effect.Error[Unit, Unit]]
  implicitly[ServiceMock.Poly3.type <:< ServiceMock.Poly.Effect.Output[Unit, Nothing]]
  implicitly[ServiceMock.Poly4.type <:< ServiceMock.Poly.Effect.InputError[Unit]]
  implicitly[ServiceMock.Poly5.type <:< ServiceMock.Poly.Effect.InputOutput[Unit]]
  implicitly[ServiceMock.Poly6.type <:< ServiceMock.Poly.Effect.ErrorOutput[Unit]]
  implicitly[ServiceMock.Poly7.type <:< ServiceMock.Poly.Effect.InputErrorOutput]
  implicitly[ServiceMock.Poly8.type <:< ServiceMock.Poly.Effect.Output[Unit, Nothing]]
  implicitly[ServiceMock.Poly9.type <:< ServiceMock.Poly.Effect.Output[Unit, Nothing]]
  implicitly[ServiceMock.Poly10.type <:< ServiceMock.Poly.Effect.InputOutput[Nothing]]
}
