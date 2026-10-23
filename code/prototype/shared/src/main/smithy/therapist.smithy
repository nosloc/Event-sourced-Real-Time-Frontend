$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

@simpleRestJson
@httpBearerAuth
service TherapistService {
    version: "1.0.0"
    operations: [
        ListTherapists
        GetTherapist
        CreateTherapist
    ]
    errors: [
        Unauthorized
    ]
}

// Operations
@readonly
@http(method: "GET", uri: "/api/therapists", code: 200)
operation ListTherapists {
    output: ListTherapistsOutput
}

@readonly
@http(method: "GET", uri: "/api/therapists/{therapistId}", code: 200)
operation GetTherapist {
    input: GetTherapistInput
    output: GetTherapistOutput
    errors: [
        TherapistNotFound
    ]
}

@http(method: "POST", uri: "/api/therapists", code: 201)
operation CreateTherapist {
    input: CreateTherapistInput
    output: CreateTherapistOutput
    errors: [
        CompanyNotFound
        TherapistAlreadyExists
        InvalidTherapist
    ]
}

// Structures
// Trigram for TherapistId: 3 lowercase letters
list Therapists {
    member: Therapist
}

structure ListTherapistsOutput {
    @required
    therapists: Therapists
}

structure GetTherapistInput {
    @httpLabel
    @required
    therapistId: TherapistId
}

structure GetTherapistOutput {
    @required
    therapist: Therapist
}

// The therapist id (trigram) is chosen by the caller, unlike company and appointment ids
structure CreateTherapistInput {
    @required
    firstName: String

    @required
    lastName: String

    @required
    therapistId: TherapistId

    @required
    companyId: CompanyId

    @required
    role: Role
}

structure CreateTherapistOutput {
    @required
    therapist: Therapist
}

@error("client")
@httpError(404)
structure TherapistNotFound {
    @required
    therapistId: TherapistId
}

@error("client")
@httpError(409)
structure TherapistAlreadyExists {}

// A blank first or last name
@error("client")
@httpError(400)
structure InvalidTherapist {}

@pattern("^[a-z]{3}$")
string TherapistId

enum Role {
    GENERAL_PRACTITIONER
    NEUROPSYCHOLOGIST
    PHYSICIAN
    PSYCHOLOGIST
}

structure Therapist {
    @required
    firstName: String

    @required
    lastName: String

    @required
    therapistId: TherapistId

    @required
    companyId: CompanyId

    @required
    role: Role
}
