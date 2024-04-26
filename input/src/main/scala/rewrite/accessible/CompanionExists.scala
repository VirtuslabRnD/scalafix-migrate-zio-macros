/*
rule = ZIOAccessibleCodeGen
 */
package rewrite.accessible

import zio._
import zio.macros.accessible

@accessible
trait ServiceCompanionExists {
  def get(key: String): UIO[Int]
}

object ServiceCompanionExists {
  def default: ServiceCompanionExists = ???
}

@accessible
trait ServiceEmptyCompanion {
  def get(key: String): UIO[Int]
}

object ServiceEmptyCompanion

@accessible
trait ServiceEmptyCompanion2 {
  def get(key: String): UIO[Int]
}

object ServiceEmptyCompanion2 {}

object TestCompanionExists {
  val get1: ZIO[ServiceCompanionExists with Any, Nothing, Int] = ServiceCompanionExists.get("foo")
  val get2: ZIO[ServiceEmptyCompanion with Any, Nothing, Int] = ServiceEmptyCompanion.get("foo")
  val get3: ZIO[ServiceEmptyCompanion2 with Any, Nothing, Int] = ServiceEmptyCompanion2.get("foo")
}
