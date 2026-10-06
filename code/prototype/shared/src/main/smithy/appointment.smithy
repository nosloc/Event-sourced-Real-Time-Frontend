$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson
use alloy#uuidFormat

// Services
@simpleRestJson
service AppointmentService {
    version: "1.0.0"
    operations: [
        ListTherapistAppointments
    ]
}

// Operations
@readonly
@http(method: "GET", uri: "/api/therapists/{therapistId}/appointments", code: 200)
operation ListTherapistAppointments {
    input: ListTherapistAppointmentsInput
    output: ListTherapistAppointmentsOutput
    errors: [
        TherapistNotFound
    ]
}

// Definitions
@uuidFormat
string AppointmentId

// Structures
list Appointments {
    member: Appointment
}

structure ListTherapistAppointmentsInput {
    @httpLabel
    @required
    therapistId: TherapistId
}

structure ListTherapistAppointmentsOutput {
    @required
    appointments: Appointments
}

structure TimeRange {
    @required
    start: Timestamp

    @required
    end: Timestamp
}

structure Appointment {
    @required
    appointmentId: AppointmentId

    @required
    therapistId: TherapistId

    @required
    timeRange: TimeRange
}
