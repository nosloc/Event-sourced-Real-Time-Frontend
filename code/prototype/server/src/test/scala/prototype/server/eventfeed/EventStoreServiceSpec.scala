package prototype.server.eventfeed

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*

import prototype.api.*
import prototype.server.App
import prototype.server.support.{PostgresSuite, TestApp}

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pages of the event store, on a real Postgres in Docker. Each company
  * created writes exactly one event, so the store holds as many events as
  * companies. Docker must be running.
  */
class EventStoreServiceSpec extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers
    with PostgresSuite:

  private val address = Address("Example Street", "1", "Exampleville", "VD")

  // Names must differ: a company name can only be used once
  private def createCompanies(app: App.Services, count: Int): IO[Unit] =
    (1 to count).toList.traverse_(i =>
      app.companies.createCompany(s"Clinic $i", address)
    )

  private def positions(page: EventsPage): List[Long] =
    page.events.map(_.position)

  "The event store service" should "return an empty page and a zero total on an empty store" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          latest <- app.eventStore.getLatestEvents(50)
          listed <- app.eventStore.listEvents(0, 50)
        yield
          latest shouldBe EventsPage(Nil, 0L)
          listed shouldBe EventsPage(Nil, 0L)
      }
    }
  }

  it should "page the events from the oldest" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          _ <- createCompanies(app, 5)
          all <- app.eventStore.listEvents(0, 5)
          first <- app.eventStore.listEvents(0, 2)
          last <- app.eventStore.listEvents(4, 2)
        yield
          all.events.map(_.eventType).distinct shouldBe List("CompanyCreated")
          positions(all) shouldBe positions(all).sorted
          positions(first) shouldBe positions(all).take(2)
          positions(last) shouldBe positions(all).drop(4)
          List(all.total, first.total, last.total) shouldBe List(5L, 5L, 5L)
      }
    }
  }

  it should "return the newest events, oldest first" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          _ <- createCompanies(app, 5)
          all <- app.eventStore.listEvents(0, 5)
          latest <- app.eventStore.getLatestEvents(2)
          more <- app.eventStore.getLatestEvents(10)
        yield
          positions(latest) shouldBe positions(all).drop(3)
          latest.total shouldBe 5L
          // Asking for more than the store holds gives everything
          positions(more) shouldBe positions(all)
      }
    }
  }

  it should "keep skip and take within bounds" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        for
          _ <- createCompanies(app, 5)
          beyond <- app.eventStore.listEvents(10, 2)
          negativeSkip <- app.eventStore.listEvents(-3, 2)
          firstTwo <- app.eventStore.listEvents(0, 2)
          negativeTake <- app.eventStore.listEvents(0, -1)
          huge <- app.eventStore.listEvents(0, 1000)
        yield
          beyond shouldBe EventsPage(Nil, 5L)
          negativeSkip shouldBe firstTwo
          negativeTake shouldBe EventsPage(Nil, 5L)
          huge.events.size shouldBe 5
      }
    }
  }

  it should "end the newest page with the newest event even when take is above the cap" in {
    freshDatabase.use { postgres =>
      TestApp.make(postgres).use { app =>
        val count = EventStoreServiceImpl.MaxTake + 5
        for
          _ <- createCompanies(app, count)
          newest <- app.eventStore.getLatestEvents(1)
          capped <- app.eventStore.getLatestEvents(EventStoreServiceImpl.MaxTake + 100)
        yield
          capped.events.size shouldBe EventStoreServiceImpl.MaxTake
          capped.events.last shouldBe newest.events.last
          capped.total shouldBe count.toLong
      }
    }
  }
end EventStoreServiceSpec
