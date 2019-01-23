package pl.edu.mimuw.sws
import java.io._
import scalaz.zio._
import scala.io.BufferedSource


object FileIO {
  def getBufferedSource(fileName: String): IO[Exception, BufferedSource] =
    IO.syncException(scala.io.Source.fromFile(fileName))

  def closeBufferedSource(bufferedSource: BufferedSource): IO[Nothing, Unit] =
    IO.sync(bufferedSource.close())

  def getPrintWriter(fileName: String): IO[Exception, PrintWriter] =
    IO.syncException({
      val fw: FileWriter = new FileWriter(fileName, true)
      new PrintWriter(fw)
    })

  def append(printWriter: PrintWriter)(string: String): IO[Exception, Unit] =
    IO.syncException({
      printWriter.println(string)
      printWriter.flush()
    })

  def closePrintWriter(printWriter: PrintWriter): IO[Nothing, Unit] =
    IO.sync(printWriter.close())
}
