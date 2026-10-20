package prototype.server.core.company.domain

import java.util.UUID

import prototype.server.UserFacingFailure

/** Explains why the company could not be created.
  *
  * @param message
  *   explanation shown to the caller
  */
enum CompanyError(val message: String)
    extends Exception(message),
      UserFacingFailure:

  /** A company already uses the given id
    *
    * @param id
    *   company id
    */
  case COMPANY_ALREADY_EXISTS(id: UUID)
      extends CompanyError(s"Company with id $id already exists.")

  /** The company name is empty
    *
    * @param id
    *   company id
    */
  case COMPANY_NAME_EMPTY(id: UUID)
      extends CompanyError(s"Company with id $id has an empty name.")

  /** The company has an invalid address
    *
    * @param id
    *   company id
    */
  case COMPANY_ADDRESS_INVALID(id: UUID)
      extends CompanyError(s"Company with id $id has an invalid address.")
