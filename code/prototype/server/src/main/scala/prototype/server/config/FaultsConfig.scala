package prototype.server.config

import pureconfig.ConfigReader

/** @param enabled
  *   whether the faults can be set (`/api/faults`); off, the routes do not
  *   exist and nothing is ever slowed down
  */
final case class FaultsConfig(enabled: Boolean = false) derives ConfigReader
