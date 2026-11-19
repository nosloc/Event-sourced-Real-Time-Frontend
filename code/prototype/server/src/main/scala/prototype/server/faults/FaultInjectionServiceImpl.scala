package prototype.server.faults

import cats.effect.IO

import prototype.api.{FaultInjectionService, Faults}

import org.typelevel.log4cats.slf4j.Slf4jLogger

/** `/api/faults`: reads and replaces the faults. Mounted only when the server
  * runs with faults enabled.
  */
final class FaultInjectionServiceImpl(injection: FaultInjection)
    extends FaultInjectionService[IO]:

  private val logger = Slf4jLogger.getLogger[IO]

  override def getFaults(): IO[Faults] = injection.current

  // Logged, so the server's output tells when a run was slowed on purpose
  override def setFaults(faults: Faults): IO[Faults] =
    logger.warn(s"Faults set: $faults") *> injection.set(faults)
