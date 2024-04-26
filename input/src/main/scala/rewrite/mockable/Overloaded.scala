/*
rule = ZIOMockableCodeGen
 */
package rewrite.mockable

import zio._
import zio.mock.{mockable, Mock}

object Overloaded {
  type OverloadedPureDefsModule = OverloadedPureDefsModule.Service
  object OverloadedPureDefsModule {
    trait Service {
      def overloaded(n: Int): IO[String, String]
      def overloaded(n: Long): IO[String, String]
    }
  }

  type OverloadedImpureDefsModule = OverloadedImpureDefsModule.Service
  object OverloadedImpureDefsModule {
    trait Service {
      def overloaded(n: Int): String
      def overloaded(n: Long): String
    }
  }
}
import Overloaded._

@mockable[OverloadedPureDefsModule]
object OverloadedPureDefsMocks

@mockable[OverloadedImpureDefsModule]
object OverloadedImpureDefsMocks

object OverloadedTest {
  import Overloaded._
  implicitly[OverloadedPureDefsMocks.type <:< Mock[OverloadedPureDefsModule]]
  implicitly[OverloadedPureDefsMocks.Overloaded._0.type <:< OverloadedPureDefsMocks.Effect[Int, String, String]]
  implicitly[OverloadedPureDefsMocks.Overloaded._1.type <:< OverloadedPureDefsMocks.Effect[Long, String, String]]

  implicitly[OverloadedImpureDefsMocks.type <:< Mock[OverloadedImpureDefsModule]]
  implicitly[OverloadedImpureDefsMocks.Overloaded._0.type <:< OverloadedImpureDefsMocks.Method[Int, Throwable, String]]
  implicitly[OverloadedImpureDefsMocks.Overloaded._1.type <:< OverloadedImpureDefsMocks.Method[Long, Throwable, String]]
}
