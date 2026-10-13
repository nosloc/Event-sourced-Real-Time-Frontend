package prototype.client.Views.Timetable

import prototype.api.*
import com.raquo.laminar.api.L.*
import prototype.client.Api.ApiCall
import prototype.client.Api

object UserInfo:
  def apply(
      currentUser: ApiCall[Therapist]
  ): Mod[HtmlElement] =
    div(
      cls := "muted",
      child <-- currentUser.stateS.map {
        case Api.Loading =>
          div(cls := "skeleton skeleton--line")
        case Api.Success(therapist) =>
          div(
            cls := "who",
            span(
              cls := "who__name",
              s"${therapist.firstName} ${therapist.lastName}"
            ),
            span(
              cls := "who__role",
              therapist.role.toString().toLowerCase().replace('_', ' ')
            ),
            span(cls := "who__id", therapist.therapistId.value)
          )
        case Api.Error(error) =>
          div(
            cls := "banner banner--error",
            s"Error loading user info: ${error.getMessage}"
          )
      }
    )
  end apply
