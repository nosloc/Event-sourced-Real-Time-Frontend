package prototype.server.core.therapist

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.therapist.therapistModel.*

/** A therapist has a single command (create). The caller chooses the id (a
  * trigram), so unlike appointments and companies a duplicate is a real case.
  */
class TherapistDeciderSpec extends AnyFlatSpec with Matchers:

  private val decider = TherapistDecider()

  private val id = TherapistId("arb")
  private val company = CompanyId(UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e"))
  private val create =
    CreateTherapistCommand(id, "Alexandre", "Robert", company, Role.PSYCHOLOGIST)

  private val existing: TherapistState =
    Some(Therapist("Alexandre", "Robert", id, company, Role.PSYCHOLOGIST))

  "decide" should "produce TherapistCreated when the therapist does not exist" in {
    decider.decide(None, create) shouldBe
      Right(
        List(
          TherapistCreated(id, "Alexandre", "Robert", company, Role.PSYCHOLOGIST)
        )
      )
  }

  it should "reject the command with ALREADY_EXISTS when the therapist exists" in {
    decider.decide(existing, create) shouldBe Left(ALREADY_EXISTS)
  }

  "evolve" should "put every field of TherapistCreated in its place" in {
    // Distinct values everywhere. The event lists the id first and the
    // Therapist lists the first name first, so a mix-up of the two orders
    // (or of first and last name) would show here.
    val state = decider.evolve(
      None,
      TherapistCreated(
        TherapistId("mdp"),
        "Marie",
        "Dupont",
        company,
        Role.NEUROPSYCHOLOGIST
      )
    )

    state shouldBe Some(
      Therapist("Marie", "Dupont", TherapistId("mdp"), company, Role.NEUROPSYCHOLOGIST)
    )
  }

  "the events produced by decide" should "build the expected state when applied with evolve" in {
    val state = decider
      .decide(None, create)
      .map(_.foldLeft(Option.empty[Therapist])(decider.evolve))

    state shouldBe Right(existing)
  }

  "the decider" should "start from no therapist" in {
    decider.initialState shouldBe None
  }
