package vishnu.adventofcode.zio

import zio._
import zio.logging._

import java.io.InputStream

trait Util {

  def openResource(fileName:String) = {
    for {
    inputstream <- ZIO.effect(this.getClass.getResourceAsStream(fileName))
      _ <- log.info(s"Opened inputstream for resource name $fileName")
    } yield {
      inputstream
    }

  }
  def closeStream(stream:InputStream)  = for {
    _ <- log.info("Closing input stream")
    _ <- ZIO.effectTotal(stream.close())
    _ <- log.info("Closed input stream")
  } yield {

  }

  def getLines(inputStream: InputStream) = ZIO.effectTotal(scala.io.Source.fromInputStream(inputStream).getLines().toSeq)
  def zManagedResource(fileName:String) = ZManaged.make(openResource(fileName))(closeStream)
  def readResource(filename:String) = {
    zManagedResource(filename).map(stream => scala.io.Source.fromInputStream(stream).getLines())
  }

}
