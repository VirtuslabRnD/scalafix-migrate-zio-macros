/*
rule = ZIOMockableCodeGen
 */
package rewrite.mockable

import zio._
import zio.stream.ZSink
import zio.mock._
import rewrite.accessible._

@mockable[ImplicitsParamsService]
object ImplicitsParamsServiceMock

object ImplicitsParamsServiceMockTest {
  implicitly[ImplicitsParamsServiceMock.type <:< Mock[ImplicitsParamsService]]
  implicitly[ImplicitsParamsServiceMock.ReduceTransactions.type <:< ImplicitsParamsServiceMock.Poly.Effect.InputErrorOutput]
  implicitly[
    ImplicitsParamsServiceMock.EventsForGroup.type <:< ImplicitsParamsServiceMock.Method[GroupId, Throwable, List[TransactionEvent]]
  ]
}
