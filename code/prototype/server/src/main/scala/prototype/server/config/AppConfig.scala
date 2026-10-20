package prototype.server.config

import persistent4s.postgres.PostgresConfig

import pureconfig.ConfigReader

final case class AppConfig(
    postgres: PostgresConfig,
    ember: EmberConfig
) derives ConfigReader
