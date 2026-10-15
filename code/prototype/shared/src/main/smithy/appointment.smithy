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
        UpdateAppointment
        GetAppointment
        CreatePrestation
        DeletePrestation
        UpdatePrestationStatus
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

@http(method: "POST", uri: "/api/appointments", code: 201)
operation CreateAppointment {
    input: CreateAppointmentInput
    output: CreateAppointmentOutput
    errors: [
        TherapistsNotFound
        InvalidTimeRange
        InvalidParticipants
        TherapistBusy
        PatientsNotFound
        PatientBusy
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

@idempotent
@http(method: "PUT", uri: "/api/appointments/{appointmentId}", code: 200)
operation UpdateAppointment {
    input: UpdateAppointmentInput
    output: UpdateAppointmentOutput
    errors: [
        AppointmentNotFound
        InvalidTimeRange
        InvalidParticipants
        TherapistsNotFound
        TherapistBusy
        PatientsNotFound
        PatientBusy
    ]
}

@readonly
@http(method: "GET", uri: "/api/appointments/{appointmentId}", code: 200)
operation GetAppointment {
    input: GetAppointmentInput
    output: GetAppointmentOutput
    errors: [
        AppointmentNotFound
    ]
}

@http(method: "POST", uri: "/api/appointments/{appointmentId}/prestations", code: 201)
operation CreatePrestation {
    input: CreatePrestationInput
    output: CreatePrestationOutput
    errors: [
        AppointmentNotFound
        InvalidPrestation
    ]
}

@idempotent
@http(method: "DELETE", uri: "/api/appointments/{appointmentId}/prestations/{prestationId}", code: 204)
operation DeletePrestation {
    input: DeletePrestationInput
    errors: [
        AppointmentNotFound
        PrestationNotFound
    ]
}

@idempotent
@http(method: "PUT", uri: "/api/appointments/{appointmentId}/prestations/{prestationId}/status", code: 200)
operation UpdatePrestationStatus {
    input: UpdatePrestationStatusInput
    output: UpdatePrestationStatusOutput
    errors: [
        AppointmentNotFound
        PrestationNotFound
        InvalidStatusChange
    ]
}
// enums

enum AppointmentType {
    NEW_PATIENT
    INDIVIDUAL
    COUPLE
    FAMILY
    GROUP
}

enum PrestationStatus {
    TO_BILL
    BILLED
    PAID
    CANCELLED
}

// Definitions
@uuidFormat
string AppointmentId

@uuidFormat
string PrestationId
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

list TherapistIds {
    member: TherapistId
}

list PatientIds {
    member: PatientId
}

structure CreateAppointmentInput {
    @required
    therapistIds: TherapistIds

    @required
    patientIds: PatientIds

    @required
    appointmentType: AppointmentType

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

@error("client")
@httpError(400)
structure InvalidParticipants {}

@error("client")
@httpError(409)
structure TherapistBusy {
    @required
    therapistIds: TherapistIds
}

@error("client")
@httpError(409)
structure PatientBusy {
    @required
    patientIds: PatientIds
}

@error("client")
@httpError(404)
structure TherapistsNotFound {
    @required
    therapistIds: TherapistIds
}

@error("client")
@httpError(404)
structure PatientsNotFound {
    @required
    patientIds: PatientIds
}

structure DeleteAppointmentInput {
    @httpLabel
    @required
    appointmentId: AppointmentId
}

@error("client")
@httpError(404)
structure AppointmentNotFound {}

structure UpdateAppointmentInput {
    @httpLabel
    @required
    appointmentId: AppointmentId

    @required
    appointmentType: AppointmentType

    @required
    therapistIds: TherapistIds

    @required
    patientIds: PatientIds

    @required
    timeRange: TimeRange
}

structure UpdateAppointmentOutput {
    @required
    appointment: Appointment
}

structure GetAppointmentInput {
    @httpLabel
    @required
    appointmentId: AppointmentId
}

structure GetAppointmentOutput {
    @required
    appointment: Appointment
}

structure CreatePrestationInput {
    @httpLabel
    @required
    appointmentId: AppointmentId

    @required
    label: String

    @required
    quantity: Integer

    @required
    unitPrice: Float
}

structure CreatePrestationOutput {
    @required
    appointment: Appointment
}

structure DeletePrestationInput {
    @httpLabel
    @required
    appointmentId: AppointmentId

    @httpLabel
    @required
    prestationId: PrestationId
}

structure UpdatePrestationStatusInput {
    @httpLabel
    @required
    appointmentId: AppointmentId

    @httpLabel
    @required
    prestationId: PrestationId

    @required
    status: PrestationStatus
}

structure UpdatePrestationStatusOutput {
    @required
    appointment: Appointment
}

@error("client")
@httpError(404)
structure PrestationNotFound {}

@error("client")
@httpError(400)
structure InvalidPrestation {}

@error("client")
@httpError(409)
structure InvalidStatusChange {}

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
    therapistIds: TherapistIds

    @required
    patientIds: PatientIds

    @required
    appointmentType: AppointmentType

    @required
    timeRange: TimeRange

    @required
    prestations: Prestations
}

structure Prestation {
    @required
    appointmentId: AppointmentId

    @required
    prestationId: PrestationId

    @required
    label: String

    @required
    quantity: Integer

    @required
    unitPrice: Float

    @required
    status: PrestationStatus
}

list Prestations {
    member: Prestation
}
