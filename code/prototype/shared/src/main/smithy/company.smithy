$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson
use alloy#uuidFormat

// Services
@simpleRestJson
service CompanyService {
    version: "1.0.0"
    operations: [
        ListCompanies
        GetCompany
    ]
}

// Operations
@readonly
@http(method: "GET", uri: "/api/companies", code: 200)
operation ListCompanies {
    output: ListCompaniesOutput
}

@readonly
@http(method: "GET", uri: "/api/companies/{companyId}", code: 200)
operation GetCompany {
    input: GetCompanyInput
    output: GetCompanyOutput
    errors: [
        CompanyNotFound
    ]
}

@uuidFormat
string CompanyId

// Structures
list Companies {
    member: Company
}

structure ListCompaniesOutput {
    @required
    companies: Companies
}

structure GetCompanyInput {
    @httpLabel
    @required
    companyId: CompanyId
}

structure GetCompanyOutput {
    @required
    company: Company
}

@error("client")
@httpError(404)
structure CompanyNotFound {}

structure Address {
    @required
    street: String

    @required
    number: String

    @required
    city: String

    @required
    state: String
}

structure Company {
    @required
    companyId: CompanyId

    @required
    name: String

    @required
    address: Address
}
