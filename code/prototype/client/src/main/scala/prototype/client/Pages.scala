package prototype.client

object Pages:
  sealed trait Page(val title: String)
  case object LoginPage extends Page("Login")
  case object HomePage extends Page("Home")
  case object TimetablePage extends Page("Timetable")
  case object AdminPage extends Page("Admin")
  case object DashboardPage extends Page("Dashboard")
  case object EventSourcingPage extends Page("Event sourcing")
  case object NotFoundPage extends Page("Not Found")
end Pages
