package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import scala.scalajs.js.Date
import prototype.api.*
import prototype.client.Api
import prototype.client.views.components.Modal
import prototype.client.views.timetable.AppointmentForm.AppointmentFormState

object CreateAppointmentModal:
  def apply(
      today: Date,
      username: String,
      showCreateAppointmentModalVar: Var[Boolean],
      therapistListS: Signal[List[Therapist]],
      patientListS: Signal[List[Patient]]
  ): Mod[HtmlElement] =
    child.maybe <-- showCreateAppointmentModalVar.signal.map(isOpen =>
      Option.when(isOpen) {
        // Fresh state each time the modal opens
        val initialState = AppointmentFormState(
          date = today.dateToString,
          start = "09:00",
          end = "10:00",
          appointmentType = AppointmentType.INDIVIDUAL,
          therapistIds = List(TherapistId(username)),
          patientIds = Nil
        )
        Modal(
          title = "Create Appointment",
          onClose = () => showCreateAppointmentModalVar.set(false)
        )(
          AppointmentForm(
            initialState = initialState,
            saveFunction = draft =>
              Api.appointments.createAppointment(
                draft.therapistIds,
                draft.patientIds,
                draft.appointmentType,
                draft.timeRange
              ),
            onSuccess = () => {
              showCreateAppointmentModalVar.set(false)
            },
            onCancel = () => showCreateAppointmentModalVar.set(false),
            submitLabel = "Create",
            therapistListS = therapistListS,
            patientListS = patientListS
          )
        )
      }
    )
