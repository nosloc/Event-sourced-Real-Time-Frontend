$version: "2.0"

namespace prototype.api

use alloy#LocalDate
use alloy#simpleRestJson

// Read-only figures for one company, computed by the server. Therapists belong
// to a company; appointments and patients are tied to it through the therapists.
@simpleRestJson
@httpBearerAuth
service DashboardService {
    version: "1.0.0"
    operations: [
        GetDirectoryDashboard
        GetActivityDashboard
        GetBillingDashboard
    ]
    errors: [
        Unauthorized
    ]
}

// Operations
@readonly
@http(method: "GET", uri: "/api/companies/{companyId}/dashboards/directory", code: 200)
operation GetDirectoryDashboard {
    input: GetDirectoryDashboardInput
    output: GetDirectoryDashboardOutput
    errors: [
        CompanyNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/api/companies/{companyId}/dashboards/activity", code: 200)
operation GetActivityDashboard {
    input: GetActivityDashboardInput
    output: GetActivityDashboardOutput
    errors: [
        CompanyNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/api/companies/{companyId}/dashboards/billing", code: 200)
operation GetBillingDashboard {
    input: GetBillingDashboardInput
    output: GetBillingDashboardOutput
    errors: [
        CompanyNotFound
    ]
}

// Shared structures
enum PeriodUnit {
    WEEK
    MONTH
}

// A number per therapist: appointments in the activity dashboard, patients in
// the directory dashboard
structure TherapistCount {
    @required
    therapistId: TherapistId

    @required
    count: Integer
}

list TherapistCounts {
    member: TherapistCount
}

// Directory
structure GetDirectoryDashboardInput {
    @httpLabel
    @required
    companyId: CompanyId
}

structure GetDirectoryDashboardOutput {
    @required
    therapists: Integer

    // Patients with at least one appointment with a therapist of the company
    @required
    patientsSeen: Integer

    // Patients with an appointment in the future or in the past 7 days
    @required
    activePatients: Integer

    @required
    appointments: Integer

    @required
    therapistsPerRole: RoleCounts

    @required
    patientsPerTherapist: TherapistCounts
}

structure RoleCount {
    @required
    role: Role

    @required
    count: Integer
}

list RoleCounts {
    member: RoleCount
}

// Activity
structure GetActivityDashboardInput {
    @httpLabel
    @required
    companyId: CompanyId

    @httpQuery("unit")
    @required
    unit: PeriodUnit

    // Any day inside the chosen week or month
    @httpQuery("date")
    @required
    date: LocalDate
}

structure GetActivityDashboardOutput {
    // First and last day of the chosen period, both included
    @required
    periodStart: LocalDate

    @required
    periodEnd: LocalDate

    @required
    appointments: Integer

    @required
    patientsSeen: Integer

    // Absent when there is no appointment in the period
    busiestTherapist: TherapistCount

    @required
    perTherapist: TherapistCounts

    // The chosen period and the 5 before it, oldest first
    @required
    evolution: PeriodCounts
}

structure PeriodCount {
    // First day of the period
    @required
    start: LocalDate

    @required
    appointments: Integer
}

list PeriodCounts {
    member: PeriodCount
}

// Billing: every prestation counts on the day of its appointment, whatever its status
structure GetBillingDashboardInput {
    @httpLabel
    @required
    companyId: CompanyId
}

structure GetBillingDashboardOutput {
    @required
    amountPerStatus: AmountPerStatus

    // The oldest appointments that still have prestations to bill
    @required
    oldestToBill: ToBillItems

    // The last 52 weeks ending with the current one, oldest first
    @required
    weekly: WeeklyAmounts
}

structure AmountPerStatus {
    @required
    toBill: Float

    @required
    billed: Float

    @required
    paid: Float

    @required
    cancelled: Float
}

structure ToBillItem {
    @required
    appointmentId: AppointmentId

    @required
    date: LocalDate

    @required
    appointmentType: AppointmentType

    @required
    therapistIds: TherapistIds

    // The amount still to bill for this appointment
    @required
    amount: Float
}

list ToBillItems {
    member: ToBillItem
}

// Cancelled prestations are left out
structure WeeklyAmount {
    // Monday of the week
    @required
    weekStart: LocalDate

    @required
    toBill: Float

    @required
    billed: Float

    @required
    paid: Float
}

list WeeklyAmounts {
    member: WeeklyAmount
}
