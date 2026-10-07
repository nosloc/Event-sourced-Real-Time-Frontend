package prototype.client

import com.raquo.waypoint.*

object Routes:

  val appRoot = root / "app"

  val routes = List(
    Route.static(Pages.LoginPage, appRoot / "login" / endOfSegments),
    Route.static(Pages.HomePage, appRoot / endOfSegments)
  )
