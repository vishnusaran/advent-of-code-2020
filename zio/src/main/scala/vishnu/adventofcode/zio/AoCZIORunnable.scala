package vishnu.adventofcode.zio

import zio.{ExitCode, URIO, ZIO}
import zio.logging._

trait AoCZIORunnable extends zio.App {
  val env =
    Logging.console(
      logLevel = LogLevel.Info,
    ) >>> Logging.withRootLoggerName("adventofcode")

  val resourceName:String

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    readResource(resourceName).use(getRunLayer).provideCustomLayer(env).exitCode
  }

  def getRunLayer(input:Iterator[String]):ZIO[zio.ZEnv with zio.logging.Logging,Any,Unit]
}
