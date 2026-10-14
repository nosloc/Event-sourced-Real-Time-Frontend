package prototype.server.core.company

import java.util.UUID
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import prototype.api.*
import prototype.server.core.company.companyModel.*

/** A company has a single command (create), so the matrix is small: create on
  * no company, and create on an existing one.
  */
class CompanyDeciderSpec extends AnyFlatSpec with Matchers:

  private val decider = CompanyDecider()

  private val id = CompanyId(UUID.fromString("13ddd512-9f19-4af3-b5a9-af79bd82411e"))
  private val address = Address("Example Street", "1", "Exampleville", "VD")
  private val create = CreateCompanyCommand(id, "Example Clinic", address)

  private val existing: CompanyState =
    Some(Company(id, "Example Clinic", address))

  "decide" should "produce CompanyCreated when the company does not exist" in {
    decider.decide(None, create) shouldBe
      Right(List(CompanyCreated(id, "Example Clinic", address)))
  }

  it should "reject the command with ALREADY_EXISTS when the company exists" in {
    decider.decide(existing, create) shouldBe Left(ALREADY_EXISTS)
  }

  "evolve" should "build the company from CompanyCreated, each field in its place" in {
    val otherAddress = Address("Sample Avenue", "2", "Sampletown", "GE")

    decider.evolve(None, CompanyCreated(id, "Demo Health Center", otherAddress)) shouldBe
      Some(Company(id, "Demo Health Center", otherAddress))
  }

  "the events produced by decide" should "build the expected state when applied with evolve" in {
    val state = decider
      .decide(None, create)
      .map(_.foldLeft(Option.empty[Company])(decider.evolve))

    state shouldBe Right(existing)
  }

  "the decider" should "start from no company" in {
    decider.initialState shouldBe None
  }
