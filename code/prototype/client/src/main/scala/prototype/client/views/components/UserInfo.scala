package prototype.client.views.components

import prototype.api.*
import com.raquo.laminar.api.L.*
import prototype.client.Api.ApiCall
import prototype.client.Api
import prototype.client.ErrorMessages.errorToMessage

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
            )
          )
        case Api.Error(error) =>
          div(
            cls := "banner banner--error",
            s"${errorToMessage(error)} (user info)"
          )
      }
    )
  end apply
