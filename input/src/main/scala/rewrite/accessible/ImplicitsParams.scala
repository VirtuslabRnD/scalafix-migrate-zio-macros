/*
rule = ZIOAccessibleCodeGen
 */
package rewrite.accessible

import zio._
import zio.stream.ZSink
import zio.macros.accessible

case class GroupId(v: Int)
trait TransactionEvent

@accessible
trait ImplicitsParamsService {
  def reduceTransactions[R: Tag, E: Tag, A: Tag](groupId: GroupId, sink: ZSink[R, E, TransactionEvent, Nothing, A]): ZIO[R, E, A]
  def eventsForGroup(id: GroupId)(implicit tag: Tag[GroupId]): List[TransactionEvent]
}

object ImplicitsParamsServiceTest {
  def sink: ZSink[Any, Nothing, TransactionEvent, Nothing, Unit] = ???
  val reduceTransactions: ZIO[ImplicitsParamsService, Nothing, Unit] = ImplicitsParamsService.reduceTransactions(GroupId(42), sink)
  val eventsForGroup: ZIO[ImplicitsParamsService, Nothing, List[TransactionEvent]] = ImplicitsParamsService.eventsForGroup(GroupId(42))
}
