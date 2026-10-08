package prototype.client

import com.raquo.waypoint.*

object AppRouter
    extends Router[Pages.Page](
      routes = Routes.routes,
      getPageTitle = page => page.title,
      serializePage = {
        case Pages.LoginPage     => "login"
        case Pages.HomePage      => "home"
        case Pages.TimetablePage => "timetable"
        case Pages.NotFoundPage  => "notfound"
      },
      deserializePage = {
        case "login"     => Pages.LoginPage
        case "home"      => Pages.HomePage
        case "timetable" => Pages.TimetablePage
        case _           => Pages.NotFoundPage
      },
      routeFallback = _ => Pages.NotFoundPage
    )
