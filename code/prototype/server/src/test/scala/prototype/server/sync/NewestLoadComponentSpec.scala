package prototype.server.sync

import java.util.UUID

import scala.concurrent.duration.*
import scala.math.Ordering.Implicits.infixOrderingOps

import cats.effect.{IO, Ref}
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*

import prototype.api.*
import prototype.server.App
import prototype.server.config.FaultsConfig
import prototype.server.core.PrototypeEventCodec
import prototype.server.core.appointment.AppointmentFixtures.range
import prototype.server.eventfeed.EventStoreServiceImpl
import prototype.server.support.{PostgresSuite, TestApp}
import prototype.shared.Cursors.given
import prototype.shared.company.CompanyCreated

import persistent4s.EventTypeName

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The newest load on a real Postgres in Docker, with the companies read model
  * slowed down (a read model fault) while a writer creates companies. Every
  * list read meanwhile, its snapshot plus its catch-up, must hold exactly the
  * companies the event store holds up to its head. Docker must be running.
  */
class NewestLoadComponentSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val address = Address("Example Street", "1", "Exampleville", "VD")

  private val codec = PrototypeEventCodec.eventCodec

  // Long enough for the lists read during the writes to lag behind them
  private val slowCompanies = Faults(
    DelayMillis(0),
    List(ReadModelDelay(ReadModel.COMPANIES, DelayMillis(150)))
  )

  private def companyId(message: EventMessage): IO[UUID] =
    IO.fromEither(
      codec.decode(EventTypeName.fromString(message.eventType), message.payload)
    ).flatMap {
      case created: CompanyCreated => IO.pure(created.id)
      case other =>
        IO.raiseError(IllegalStateException(s"Not a company creation: $other"))
    }

  // Lists the companies until `done`, then once more
  private def listUntil(
      app: App.Services,
      done: Ref[IO, Boolean]
  ): IO[List[ListCompaniesOutput]] =
    def loop(read: List[ListCompaniesOutput]): IO[List[ListCompaniesOutput]] =
      done.get.flatMap(finished =>
        app.companies.listCompanies().flatMap(answer =>
          if finished then IO.pure((answer :: read).reverse)
          else IO.sleep(100.millis) *> loop(answer :: read)
        )
      )
    loop(Nil)

  "A list of companies" should "hold every company up to its head, and each once, while its read model lags" in {
    freshDatabase.use { postgres =>
      App
        .makeWithConfig(
          TestApp.config(postgres).copy(faults = FaultsConfig(enabled = true))
        )
        .use { app =>
          for
            _ <- app.faults.set(slowCompanies)
            done <- Ref.of[IO, Boolean](false)
            writer <- (1 to 12).toList
              .traverse_(i => app.companies.createCompany(s"Clinic $i", address))
              .guarantee(done.set(true))
              .start
            answers <- listUntil(app, done)
            _ <- writer.joinWithNever
            stored <- app.eventStore.listEvents(0L, EventStoreServiceImpl.MaxTake)
            created = stored.events.filter(_.eventType == "CompanyCreated")
            checked <- answers.traverse { answer =>
              for
                caughtUp <- answer.catchUp.events.traverse(companyId)
                expected <- created
                  .filter(_.cursor <= answer.catchUp.head)
                  .traverse(companyId)
              yield (
                answer.companies.map(_.companyId.value) ++ caughtUp,
                expected
              )
            }
          yield
            // The snapshot plus the catch-up, against the store up to the head:
            // the same companies, none twice
            checked.foreach((shown, expected) =>
              shown.sorted shouldBe expected.sorted
            )
            // Otherwise the case proves nothing about the catch-up
            answers.exists(_.catchUp.events.nonEmpty) shouldBe true
        }
    }
  }

  // An update that adds a therapist writes `AppointmentUpdated`, which the
  // appointments read model saves, then `TherapistAddedToAppointment`, which it
  // skips but the browser's lists decode. The update waits for the read model,
  // so its checkpoint stops on the update, below the added-to event
  "A therapist's list of appointments" should "send the added-to event of a therapist who joins, and end its head on it" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          company <- app.companies.createCompany("Example Clinic", address).map(_.companyId)
          _ <- app.therapists.createTherapist("Ann", "Lee", TherapistId("abc"), company, Role.PHYSICIAN)
          _ <- app.therapists.createTherapist("Bob", "Ray", TherapistId("xyz"), company, Role.PSYCHOLOGIST)
          created <- app.appointments
            .createAppointment(List(TherapistId("abc")), Nil, AppointmentType.INDIVIDUAL, range(10, 11))
            .map(_.appointment)
          _ <- app.appointments.updateAppointment(
            created.appointmentId,
            AppointmentType.INDIVIDUAL,
            List(TherapistId("abc"), TherapistId("xyz")),
            Nil,
            range(10, 11)
          )
          listed <- app.appointments.listTherapistAppointments(TherapistId("xyz"))
          stored <- app.eventStore.listEvents(0L, EventStoreServiceImpl.MaxTake)
          joined = stored.events.filter(_.eventType == "TherapistAddedToAppointment").last
        yield
          listed.appointments.map(_.appointmentId) shouldBe List(created.appointmentId)
          listed.catchUp.events.map(_.eventType) should contain("TherapistAddedToAppointment")
          // Every event the browser may have applied is at or below the head
          listed.catchUp.head should be >= joined.cursor
      }
    }
  }
end NewestLoadComponentSpec
