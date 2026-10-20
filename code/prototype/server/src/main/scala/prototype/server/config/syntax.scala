package prototype.server.config

import scala.reflect.ClassTag

import cats.effect.IO
import cats.effect.kernel.Resource

import pureconfig.{
  ConfigReader,
  ConfigSource
}
import pureconfig.error.ConfigReaderException

object syntax:
  extension (source: ConfigSource)
    def loadIO[A](using
        reader: ConfigReader[A],
        tag: ClassTag[A]
    ): Resource[IO, A] =
      IO.pure(source.load[A])
        .flatMap:
          case Left(errors) => IO.raiseError[A](ConfigReaderException(errors))
          case Right(value) => IO.pure(value)
        .toResource
