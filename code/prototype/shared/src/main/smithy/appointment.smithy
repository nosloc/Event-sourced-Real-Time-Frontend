$version: "2.0"

namespace prototype.api

use alloy#simpleRestJson
use alloy#uuidFormat

// Services
@simpleRestJson
@httpBearerAuth
service AppointmentService {
    version: "1.0.0"
    operations: [
        ListTherapistAppointments
        CreateAppointment
        DeleteAppointment
    ]
    errors: [
        Unauthorized
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

@http(method: "POST", uri: "/api/therapists/{therapistId}/appointments", code: 201)
operation CreateAppointment {
    input: CreateAppointmentInput
    output: CreateAppointmentOutput
    errors: [
        TherapistNotFound
        InvalidTimeRange
    ]
}

@idempotent
@http(method: "DELETE", uri: "/api/appointments/{appointmentId}", code: 204)
operation DeleteAppointment {
    input: DeleteAppointmentInput
    errors: [
        AppointmentNotFound
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

structure CreateAppointmentInput {
    @httpLabel
    @required
    therapistId: TherapistId

    @required
    timeRange: TimeRange
}

structure CreateAppointmentOutput {
    @required
    appointment: Appointment
}

@error("client")
@httpError(400)
structure InvalidTimeRange {}

structure DeleteAppointmentInput {
    @httpLabel
    @required
    appointmentId: AppointmentId
}

@error("client")
@httpError(404)
structure AppointmentNotFound {}

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
