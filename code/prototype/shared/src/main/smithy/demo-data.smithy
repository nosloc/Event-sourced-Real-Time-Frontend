$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson

// Fills an empty database with generated demo data (companies, therapists,
// patients, appointments, prestations). Only the dev account exists at boot.
@simpleRestJson
@httpBearerAuth
service DemoDataService {
    version: "1.0.0"
    operations: [
        GenerateDemoData
    ]
    errors: [
        Unauthorized
    ]
}

// Operations
@http(method: "POST", uri: "/api/demo-data", code: 200)
operation GenerateDemoData {
    output: GenerateDemoDataOutput
    errors: [
        DemoDataAlreadyGenerated
    ]
}

// Structures

// What was written (refused writes are not counted, see skippedWrites)
structure GenerateDemoDataOutput {
    @required
    companies: Integer

    @required
    therapists: Integer

    @required
    therapistsWithAppointments: Integer

    @required
    patients: Integer

    // Every appointment type, 0 included
    @required
    appointmentsByType: AppointmentTypeCounts

    // Every prestation status, 0 included: the status each prestation reached
    @required
    prestationsByStatus: PrestationStatusCounts

    // Writes a command refused, with the writes that depended on them
    @required
    skippedWrites: Integer

    @required
    durationMillis: Long
}

list AppointmentTypeCounts {
    member: AppointmentTypeCount
}

structure AppointmentTypeCount {
    @required
    appointmentType: AppointmentType

    @required
    count: Integer
}

list PrestationStatusCounts {
    member: PrestationStatusCount
}

structure PrestationStatusCount {
    @required
    status: PrestationStatus

    @required
    count: Integer
}

// Errors

// Some therapist other than dev exists, or a generation is running
@error("client")
@httpError(409)
structure DemoDataAlreadyGenerated {}
