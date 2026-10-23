package prototype.server.demo

/** How much demo data one generation creates. Change the values here; they are
  * tuned so that one generation stays under 30 seconds.
  */
object DemoDataSizes:

  // Companies created, besides the dev account's company
  val Companies: Int = 3

  // Therapists per company, drawn uniformly between the two
  val TherapistsPerCompanyMin: Int = 3
  val TherapistsPerCompanyMax: Int = 6

  // Patients in each therapist's caseload (a patient has one therapist)
  val PatientsPerTherapist: Int = 12

  // Chance that a therapist has appointments at all
  val ShareOfTherapistsWithAppointments: Double = 0.9

  // Appointments cover every weekday from this many weeks ago to this many
  // weeks ahead
  val HistoryWeeks: Int = 13
  val FutureWeeks: Int = 4

  // Appointments per weekday, drawn uniformly between the two (fewer if the
  // day is full)
  val AppointmentsPerDayMin: Int = 2
  val AppointmentsPerDayMax: Int = 5

  // Chance that a past appointment has two prestations instead of one
  val ShareWithTwoPrestations: Double = 0.2

  // Chance that a prestation ends CANCELLED instead of its age-based status
  val ShareCancelled: Double = 0.05

  // Chance that a prestation ignores its age and ends in any other status
  // (an old one still TO_BILL, a recent one already PAID, ...)
  val ShareOffTheRule: Double = 0.15

  // A prestation stays TO_BILL for this many days after its appointment, then
  // is BILLED until this many days, then PAID
  val ToBillDays: Int = 7
  val BilledDays: Int = 28

  // Therapists whose appointments are written at the same time
  val ParallelTherapists: Int = 6
