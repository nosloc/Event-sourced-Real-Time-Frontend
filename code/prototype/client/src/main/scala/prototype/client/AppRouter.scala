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
        case Pages.AdminPage     => "admin"
        case Pages.DashboardPage => "dashboards"
        case Pages.EventSourcingPage => "event-sourcing"
        case Pages.NotFoundPage  => "notfound"
      },
      deserializePage = {
        case "login"      => Pages.LoginPage
        case "home"       => Pages.HomePage
        case "timetable"  => Pages.TimetablePage
        case "admin"      => Pages.AdminPage
        case "dashboards" => Pages.DashboardPage
        case "event-sourcing" => Pages.EventSourcingPage
        case _            => Pages.NotFoundPage
      },
      routeFallback = _ => Pages.NotFoundPage
    )
