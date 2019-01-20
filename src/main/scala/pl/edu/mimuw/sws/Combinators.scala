package pl.edu.mimuw.sws
import scalaz.zio._


object Combinators {
  def insist[A, E](p: IO[E, A])(i: E => IO[Nothing, Any]): IO[Nothing, A] = {
    lazy val pi: IO[Nothing, A] = p.catchAll(e => i(e) *> pi)
    pi
  }
}
