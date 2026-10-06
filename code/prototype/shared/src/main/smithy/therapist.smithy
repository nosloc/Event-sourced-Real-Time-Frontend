$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

@simpleRestJson
service TherapistService {
    version: "1.0.0"
    operations: [
        ListTherapists
        GetTherapist
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

@error("client")
@httpError(404)
structure TherapistNotFound {}

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
