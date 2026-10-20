package prototype.server.core.dashboards

import java.time.Instant
import java.time.temporal.ChronoUnit

import prototype.api.*

object DirectoryDashboard:
  def apply(
      therapists: List[Therapist],
      companyAppointments: List[Appointment],
      now: Instant
  ): GetDirectoryDashboardOutput =
    val patientIds = getCompanyPatientIds(companyAppointments)
    val activePatients = getActivePatients(companyAppointments, now)
    val therapistsPerRole = getTherapistRoleCounts(therapists)
    val patientsPerTherapist = getPatientsPerTherapist(
      companyAppointments,
      therapists
    )

    GetDirectoryDashboardOutput(
      therapists = therapists.size,
      patientsSeen = patientIds.size,
      activePatients = activePatients.size,
      appointments = companyAppointments.size,
      therapistsPerRole = therapistsPerRole,
      patientsPerTherapist = patientsPerTherapist
    )
  end apply

  private def getCompanyPatientIds(
      companyAppointments: List[Appointment]
  ): List[PatientId] =
    companyAppointments
      .flatMap(_.patientIds)
      .distinct
  end getCompanyPatientIds

  // Patients with an appointment in the future or in the past 7 days
  private def getActivePatients(
      companyAppointments: List[Appointment],
      now: Instant
  ): List[PatientId] =
    val since = now.minus(7, ChronoUnit.DAYS).getEpochSecond
    companyAppointments
      .filter(_.timeRange.end.epochSecond >= since)
      .flatMap(_.patientIds)
      .distinct
  end getActivePatients

  // Every therapist of the company, including those with no patient yet
  private def getPatientsPerTherapist(
      companyAppointments: List[Appointment],
      companyTherapists: List[Therapist]
  ): List[TherapistCount] =
    companyTherapists.map { therapist =>
      val patientIds = companyAppointments
        .filter(_.therapistIds.contains(therapist.therapistId))
        .flatMap(_.patientIds)
        .distinct
      TherapistCount(therapist.therapistId, patientIds.size)
    }
  end getPatientsPerTherapist

  private def getTherapistRoleCounts(
      companyTherapists: List[Therapist]
  ): List[RoleCount] =
    companyTherapists
      .groupBy(_.role)
      .map { case (role, therapists) =>
        RoleCount(role, therapists.size)
      }
      .toList
  end getTherapistRoleCounts
