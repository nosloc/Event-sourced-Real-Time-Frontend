$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

@simpleRestJson
service TherapistService {
    version: "1.0.0"
    operations: [
        GetTherapists
    ]
}

structure Company {
    @required
    name: String

    @required
    address: String
}

structure Therapist {
    @required
    firstName: String

    @required
    lastName: String

    @required
    trigram: String

    @required
    company: Company
}

list TherapistList {
    member: Therapist
}

structure GetTherapistsOutput {
    @required
    therapists: TherapistList
}

@readonly
@http(method: "GET", uri: "/api/therapists", code: 200)
operation GetTherapists {
    output: GetTherapistsOutput
}
