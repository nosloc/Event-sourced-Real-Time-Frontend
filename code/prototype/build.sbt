import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion := "3.8.4"

// Scalafix's semantic rules (OrganizeImports) read the compiler's SemanticDB output
ThisBuild / semanticdbEnabled := true

lazy val root =
  project
    .in(file("."))
    .aggregate(server, client, shared.js, shared.jvm)
    .settings(
      name := "prototype"
    )
    .settings(commonSettings)
    .settings(noPublish)

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .enablePlugins(Smithy4sCodegenPlugin)
  .settings(commonSettings)
  .settings(
    Compile / smithy4sInputDirs := Seq(
      baseDirectory.value.getParentFile / "src" / "main" / "smithy"
    ),
    libraryDependencies ++= Seq(
      // %%% so each platform (JVM and JS) gets its own build of the library
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % smithy4sVersion.value
    )
  )

lazy val server =
  project
    .in(file("server"))
    .enablePlugins(JavaAppPackaging)
    .settings(commonSettings)
    .settings(
      Compile / run / fork := true
    )
    .settings(
      libraryDependencies ++= List(
        // Http4s web server framework (brings cats-effect, the IO type, with it)
        "org.http4s" %% "http4s-ember-server" % Versions.Http4s,
        "org.http4s" %% "http4s-dsl" % Versions.Http4s,
        // Swagger UI for the generated API (JVM only, so it lives here, not in shared)
        "com.disneystreaming.smithy4s" %% "smithy4s-http4s-swagger" % smithy4sVersion.value,
        // Logging
        "org.typelevel" %% "log4cats-slf4j" % Versions.Log4Cats,
        "ch.qos.logback" % "logback-classic" % Versions.Logback,
        // Tests: ScalaTest, and support for tests that return an IO
        "org.scalatest" %% "scalatest" % Versions.ScalaTest % Test,
        "org.typelevel" %% "cats-effect-testing-scalatest" % Versions.CatsEffectTesting % Test,

        // Persistent4s: Event sourcing on top of Postgres,
        "io.github.persistent4s" %% "persistent4s-core" % Versions.Persistent4s,
        "io.github.persistent4s" %% "persistent4s-circe" % Versions.Persistent4s,
        "io.github.persistent4s" %% "persistent4s-postgres" % Versions.Persistent4s
      )
    )
    .dependsOn(shared.jvm)

lazy val client =
  project
    .in(file("client"))
    .enablePlugins(ScalaJSPlugin)
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= List(
        "com.raquo" %%% "laminar" % Versions.Laminar,
        "org.http4s" %%% "http4s-dom" % Versions.Http4sDom,
        "com.raquo" %%% "waypoint" % Versions.Waypoint
      ),
      scalaJSUseMainModuleInitializer := true,

      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.ESModule)
          .withModuleSplitStyle(
            ModuleSplitStyle.SmallModulesFor(List("prototype"))
          )
      }
    )
    .dependsOn(shared.js)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    // "-feature",
    "-language:implicitConversions",
    // Report unused code (imports first), and make every warning an error
    "-Wunused:all"
  )
)

lazy val noPublish = Seq(
  publishLocal / skip := true,
  publish / skip := true
)
