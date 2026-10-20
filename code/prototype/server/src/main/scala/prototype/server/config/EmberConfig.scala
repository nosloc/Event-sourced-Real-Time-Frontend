package prototype.server.config

import com.comcast.ip4s.{Host, Port}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

final case class EmberConfig(host: Host, port: Port, appHost: Host)
    derives ConfigReader

object EmberConfig:

  given hostReader: ConfigReader[Host] =
    ConfigReader[String].emap(value =>
      Host
        .fromString(value)
        .toRight(CannotConvert(value, "Host", s"Invalid host: $value"))
    )

  given portReader: ConfigReader[Port] =
    ConfigReader[Int].emap(value =>
      Port
        .fromInt(value)
        .toRight(CannotConvert(value.toString, "Port", s"Invalid port: $value"))
    )
