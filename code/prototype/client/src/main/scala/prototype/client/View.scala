package prototype.client

import com.raquo.laminar.api.L.*
import prototype.client.Pages.*

object View:
  private val effectivePageS: Signal[Page] =
    AppRouter.currentPageSignal.combineWith(Session.currentUserS).map {
      case (HomePage, Some(_)) => HomePage
      case (HomePage, None)    => LoginPage
      case (page, _)           => page
    }

  val views: Signal[HtmlElement] =
    effectivePageS.splitMatchOne
      .handleValue(HomePage)(HomeView())
      .handleValue(LoginPage)(LoginView())
      .handleValue(NotFoundPage)(div(h1("404 - Not Found")))
      .toSignal

  val changeUrlOnDiffer: Mod[HtmlElement] =
    effectivePageS.combineWith(AppRouter.currentPageSignal) -->
      {
        case (effectivePage, currentPage) if effectivePage != currentPage =>
          AppRouter.replaceState(effectivePage)
        case _ => ()
      }
