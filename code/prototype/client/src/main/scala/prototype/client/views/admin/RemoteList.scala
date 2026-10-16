package prototype.client.views.admin

import cats.effect.IO
import com.raquo.laminar.api.L.*
import prototype.client.Api
import prototype.client.Api.ApiCall
import prototype.client.views.timetable.ErrorMessages

/** A list fetched from the server that keeps the last rows on screen while it
  * reloads, so a table never blinks. `itemsVar` is None only before the first
  * successful load.
  */
final class RemoteList[A](fetch: => IO[List[A]]):

  val itemsVar: Var[Option[List[A]]] = Var(None)
  val errorVar: Var[Option[String]] = Var(None)
  private val reloadVar: Var[Int] = Var(0)

  def reload(): Unit = reloadVar.update(_ + 1)

  // Mount this on the page: it runs the fetch on open and after each reload()
  val keepLoaded: Mod[HtmlElement] =
    reloadVar.signal.flatMapSwitch(_ =>
      ApiCall.fromIOCallSimple(fetch).stateS
    ) --> {
      case Api.Loading =>
        // Keep the previous rows to avoid flickering
        ()
      case Api.Success(items) =>
        itemsVar.set(Some(items))
        errorVar.set(None)
      case Api.Error(error) =>
        errorVar.set(Some(ErrorMessages.errorToMessage(error)))
    }
end RemoteList
