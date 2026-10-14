package prototype.server.core.therapist

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.company.CompanyServiceImpl
import prototype.server.eventStore.InMemoryEventStore

/** The therapist service end to end. A therapist must belong to a company that
  * exists, so the real company service is used (one store for both, as in the
  * server) and a company is created first.
  */
class TherapistServiceSpec
    extends AsyncFlatSpec
    with AsyncIOSpec
    with Matchers:

  private val arb = TherapistId("arb")
  private val mdp = TherapistId("mdp")
  private val tst = TherapistId("tst")

  private class Setup(
      val companies: CompanyService[IO],
      val therapists: TherapistService[IO],
      val companyId: CompanyId
  )

  // Fresh services and store per test, with one company ready to use
  private val setup: IO[Setup] =
    for
      store <- InMemoryEventStore.create
      companies <- CompanyServiceImpl(store)
      therapists <- TherapistServiceImpl(store, companies)
      company <- companies.createCompany(
        "Example Clinic",
        Address("Example Street", "1", "Exampleville", "VD")
      )
    yield new Setup(companies, therapists, company.company.companyId)

  "createTherapist" should "return the therapist with the given details" in {
    val test =
      for
        s <- setup
        created <- s.therapists.createTherapist(
          "Alexandre",
          "Robert",
          arb,
          s.companyId,
          Role.PSYCHOLOGIST
        )
      yield (created, s.companyId)

    test.asserting { case (created, companyId) =>
      created.therapist shouldBe
        Therapist("Alexandre", "Robert", arb, companyId, Role.PSYCHOLOGIST)
    }
  }

  it should "make the therapist available through getTherapist" in {
    val test =
      for
        s <- setup
        created <- s.therapists.createTherapist(
          "Alexandre",
          "Robert",
          arb,
          s.companyId,
          Role.PSYCHOLOGIST
        )
        fetched <- s.therapists.getTherapist(arb)
      yield (created, fetched)

    test.asserting { case (created, fetched) =>
      fetched.therapist shouldBe created.therapist
    }
  }

  it should "fail with CompanyNotFound for an unknown company, and store nothing" in {
    val unknownCompany = CompanyId(java.util.UUID.randomUUID())
    val test =
      for
        s <- setup
        result <- s.therapists
          .createTherapist("Alexandre", "Robert", arb, unknownCompany, Role.PSYCHOLOGIST)
          .attempt
        afterwards <- s.therapists.getTherapist(arb).attempt
      yield (result, afterwards)

    test.asserting { case (result, afterwards) =>
      result shouldBe Left(CompanyNotFound())
      afterwards shouldBe Left(TherapistNotFound(arb))
    }
  }

  it should "fail with TherapistAlreadyExists for a taken trigram, and keep the first therapist" in {
    val test =
      for
        s <- setup
        _ <- s.therapists.createTherapist(
          "Alexandre",
          "Robert",
          arb,
          s.companyId,
          Role.PSYCHOLOGIST
        )
        second <- s.therapists
          .createTherapist("Someone", "Else", arb, s.companyId, Role.PHYSICIAN)
          .attempt
        fetched <- s.therapists.getTherapist(arb)
      yield (second, fetched)

    test.asserting { case (second, fetched) =>
      second shouldBe Left(TherapistAlreadyExists())
      // The rejected command did not overwrite anything
      fetched.therapist.firstName shouldBe "Alexandre"
      fetched.therapist.role shouldBe Role.PSYCHOLOGIST
    }
  }

  it should "keep different therapists apart" in {
    val test =
      for
        s <- setup
        _ <- s.therapists.createTherapist("Alexandre", "Robert", arb, s.companyId, Role.PSYCHOLOGIST)
        _ <- s.therapists.createTherapist("Marie", "Dupont", mdp, s.companyId, Role.PHYSICIAN)
        first <- s.therapists.getTherapist(arb)
        second <- s.therapists.getTherapist(mdp)
      yield (first, second)

    test.asserting { case (first, second) =>
      first.therapist.firstName shouldBe "Alexandre"
      second.therapist.firstName shouldBe "Marie"
    }
  }

  "getTherapist" should "fail with TherapistNotFound for an unknown trigram" in {
    val unknown = TherapistId("zzz")
    val test = setup.flatMap(_.therapists.getTherapist(unknown))

    test.attempt.asserting(_ shouldBe Left(TherapistNotFound(unknown)))
  }

  "listTherapists" should "be empty before any therapist is created" in {
    setup
      .flatMap(_.therapists.listTherapists())
      .asserting(_.therapists shouldBe Nil)
  }

  it should "list the therapists sorted by id, whatever the creation order" in {
    // Created as tst, arb, mdp: a map would not keep that order, and the list
    // is shown to users, so it is sorted
    val test =
      for
        s <- setup
        _ <- s.therapists.createTherapist("T", "One", tst, s.companyId, Role.PSYCHOLOGIST)
        _ <- s.therapists.createTherapist("A", "Two", arb, s.companyId, Role.PSYCHOLOGIST)
        _ <- s.therapists.createTherapist("M", "Three", mdp, s.companyId, Role.PSYCHOLOGIST)
        listed <- s.therapists.listTherapists()
      yield listed.therapists.map(_.therapistId.value)

    test.asserting(_ shouldBe List("arb", "mdp", "tst"))
  }
