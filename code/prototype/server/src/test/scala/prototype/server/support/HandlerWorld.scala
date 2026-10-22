package prototype.server.support

import java.time.LocalDate
import java.util.UUID

import cats.effect.IO
import cats.syntax.all.*

import prototype.api.{Address, Role}
import prototype.server.core.PrototypeEvent
import prototype.server.core.company.commands.CreateCompany
import prototype.server.core.patient.commands.CreatePatient
import prototype.server.core.therapist.commands.CreateTherapist

import persistent4s.CommandRuntime

/** A fresh in-memory event store holding one company, three therapists and two
  * patients, all created through their real handlers, for the specs of the
  * commands that need people to exist (appointments).
  */
final case class HandlerWorld(
    store: InMemoryEventStore[PrototypeEvent],
    runtime: CommandRuntime[IO, PrototypeEvent]
)

object HandlerWorld:

  val company: UUID = UUID.fromString("00000000-0000-0000-0000-0000000000f1")
  val therapists: List[String] = List("abc", "xyz", "qrs")
  val patient1: UUID = UUID.fromString("00000000-0000-0000-0000-0000000000c1")
  val patient2: UUID = UUID.fromString("00000000-0000-0000-0000-0000000000c2")

  def make: IO[HandlerWorld] =
    for
      store <- InMemoryEventStore.make[PrototypeEvent]
      runtime = CommandRuntime.eventStoreOnly[IO, PrototypeEvent](store)
      _ <- runtime
        .execute(
          CreateCompany.Handler,
          CreateCompany(
            company,
            "Example Clinic",
            Address("Example Street", "1", "Exampleville", "VD")
          )
        )
        .flatMap(IO.fromEither)
      _ <- therapists.traverse_(trigram =>
        runtime
          .execute(
            CreateTherapist.Handler,
            CreateTherapist(trigram, "First", "Last", company, Role.PSYCHOLOGIST)
          )
          .flatMap(IO.fromEither)
      )
      _ <- List(
        patient1 -> LocalDate.of(1990, 1, 1),
        patient2 -> LocalDate.of(1985, 3, 12)
      ).traverse_ { (id, born) =>
        runtime
          .execute(
            CreatePatient.Handler,
            CreatePatient(id, "First", s"Last $id", born)
          )
          .flatMap(IO.fromEither)
      }
    yield HandlerWorld(store, runtime)
