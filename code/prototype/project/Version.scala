object Versions {

  // -- Frontend --

  // Laminar 17.x is the latest stable line (18.x is milestone-only for now).
  // Your laminar-playground uses 17.0.0; 17.2.1 is the same API with fixes.
  val Laminar = "17.2.1"

  val Http4sDom = "0.2.11"

  val Waypoint = "10.0.0-M1"

  // -- Backend --

  val Http4s = "0.23.38"

  val Log4Cats = "2.8.0"

  val Logback = "1.6.5"

  val Persistent4s = "0.4.0-RC4"

  // -- Tests (server) --

  val ScalaTest = "3.2.19"

  // Lets a ScalaTest test return an IO (AsyncIOSpec)
  val CatsEffectTesting = "1.6.0"

  // Starts a Postgres container for the tests that need a real database
  val Testcontainers = "1.21.3"

}
