package prototype.client.Views

import com.raquo.laminar.api.L.*
import prototype.client.Pages.*

import prototype.client.{AppRouter, Session}

object View:

  // The router's page and who is logged in. This is the only place that turns
  // "maybe a user" into a plain username: the views below receive a String.
  // Pages that need a user: HomePage and TimetablePage.
  private val pageAndUserS: Signal[(Page, Option[String])] =
    AppRouter.currentPageSignal.combineWith(Session.currentUserS)

  private val effectivePageS: Signal[Page] =
    pageAndUserS.map {
      case (HomePage | TimetablePage, None) => LoginPage
      case (page, _)                        => page
    }

  // handleCase needs its types spelled out: input pair, extracted value, view
  private type PageAndUser = (Page, Option[String])

  val views: Signal[HtmlElement] =
    pageAndUserS.splitMatchOne
      .handleCase[PageAndUser, String, HtmlElement] {
        case (HomePage, Some(username)) => username
      }((username, _) => Shell(HomeView(username), username))
      .handleCase[PageAndUser, String, HtmlElement] {
        case (TimetablePage, Some(username)) => username
      }((username, _) => Shell(TimetableView(username), username))
      .handleCase[PageAndUser, Unit, HtmlElement] {
        case (LoginPage, _) | (HomePage | TimetablePage, None) => ()
      }((_, _) => LoginView())
      .handleCase[PageAndUser, Unit, HtmlElement] { case (NotFoundPage, _) =>
        ()
      }((_, _) => div(h1("404 - Not Found")))
      .toSignal

  val changeUrlOnDiffer: Mod[HtmlElement] =
    effectivePageS.combineWith(AppRouter.currentPageSignal) -->
      {
        case (effectivePage, currentPage) if effectivePage != currentPage =>
          AppRouter.replaceState(effectivePage)
        case _ => ()
      }
