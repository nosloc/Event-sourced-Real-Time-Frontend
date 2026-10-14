package prototype.client.views.timetable

import com.raquo.laminar.api.L.*
import prototype.client.Api.*
import prototype.api.*
import prototype.client.views.components.PopoverPicker
import prototype.client.views.components.PopoverPicker.Item

/** Choose a therapist by trigram: the generic picker fed with the therapists,
  * shown as "First Last (trigram)" and as the trigram on the closed button.
  */
object TherapistPicker:
  def apply(
      therapistsVar: ApiCall[List[Therapist]],
      selectedTherapistVar: Var[String]
  ): HtmlElement =
    PopoverPicker(
      id = "therapist",
      label = "Therapist",
      items = therapistsVar.successS.map(
        _.getOrElse(List.empty).map { therapist =>
          Item(
            value = therapist.therapistId.value,
            name = s"${therapist.firstName} ${therapist.lastName}",
            hint = Some(therapist.therapistId.value),
            buttonLabel = Some(therapist.therapistId.value)
          )
        }
      ),
      selectedVar = selectedTherapistVar
    )
