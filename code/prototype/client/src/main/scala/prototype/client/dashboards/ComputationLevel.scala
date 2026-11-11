package prototype.client.dashboards

/** Where the dashboards' figures are computed: reloaded from the server on the
  * events that matter (level 0), or computed in the browser from the live
  * sources (level 1).
  */
enum ComputationLevel:
  case Server
  case Browser
