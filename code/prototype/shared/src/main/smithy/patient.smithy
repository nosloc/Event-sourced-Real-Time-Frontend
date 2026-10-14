$version: "2.0"

namespace prototype.api

use alloy#LocalDate
use alloy#simpleRestJson
use alloy#uuidFormat

// Services
@simpleRestJson
@httpBearerAuth
service PatientService {
    version: "1.0.0"
    operations: [
        CreatePatient
        GetPatient
        ListPatients
    ]
    errors: [
        Unauthorized
    ]
}

// Operations
@http(method: "POST", uri: "/api/patients", code: 201)
operation CreatePatient {
    input: CreatePatientInput
    output: CreatePatientOutput
    errors: [
        PatientAlreadyExists
    ]
}

@readonly
@http(method: "GET", uri: "/api/patients/{patientId}", code: 200)
operation GetPatient {
    input: GetPatientInput
    output: GetPatientOutput
    errors: [
        PatientNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/api/patients", code: 200)
operation ListPatients {
    output: ListPatientsOutput
}

// Structures
structure CreatePatientInput {
    @required
    firstName: String

    @required
    lastName: String

    @required
    dateOfBirth: LocalDate
}

structure CreatePatientOutput {
    @required
    patientId: PatientId
}

@error("client")
@httpError(409)
structure PatientAlreadyExists {}

structure GetPatientInput {
    @httpLabel
    @required
    patientId: PatientId
}

structure GetPatientOutput {
    @required
    patient: Patient
}

@error("client")
@httpError(404)
structure PatientNotFound {
    @required
    patientId: PatientId
}

structure ListPatientsOutput {
    @required
    patients: Patients
}

@uuidFormat
string PatientId

list Patients {
    member: Patient
}

structure Patient {
    @required
    id: PatientId

    @required
    firstName: String

    @required
    lastName: String

    @required
    dateOfBirth: LocalDate
}
