/*
rule = ZIOMockableCodeGen
 */
package rewrite.mockable

import zio._
import zio.stream._
import zio.mock.{mockable, Mock}

object ZStreams {
  type StreamDefsModule = StreamDefsModule.Service
  object StreamDefsModule {
    trait Service {
      val static: ZStream[Any, String, String]
      def zeroParams: ZStream[Any, String, String]
      def zeroParamsWithParens(): ZStream[Any, String, String]
      def singleParam(a: Int): ZStream[Any, String, String]
      def manyParams(a: Int, b: String, c: Long): ZStream[Any, String, String]
      def manyParamLists(a: Int)(b: String)(c: Long): ZStream[Any, String, String]
    }
  }
}

@mockable[ZStreams.StreamDefsModule.Service]
object ZStreamsMock

object TestZStreams {
  import ZStreams.StreamDefsModule.Service
  implicitly[ZStreamsMock.type <:< Mock[Service]]
  implicitly[ZStreamsMock.Static.type <:< ZStreamsMock.Stream[Unit, String, String]]
  implicitly[ZStreamsMock.ZeroParams.type <:< ZStreamsMock.Stream[Unit, String, String]]
  implicitly[ZStreamsMock.ZeroParamsWithParens.type <:< ZStreamsMock.Stream[Unit, String, String]]
  implicitly[ZStreamsMock.SingleParam.type <:< ZStreamsMock.Stream[Int, String, String]]
  implicitly[ZStreamsMock.ManyParams.type <:< ZStreamsMock.Stream[(Int, String, Long), String, String]]
  implicitly[ZStreamsMock.ManyParamLists.type <:< ZStreamsMock.Stream[(Int, String, Long), String, String]]
}
