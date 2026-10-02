import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion := "3.8.4"

lazy val root =
  project
    .in(file("."))
    .aggregate(server, client)
    .settings(
      name := "prototype"
    )
    .settings(commonSettings)
    .settings(noPublish)

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(commonSettings)

lazy val server =
  project
    .in(file("server"))
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= List(
        // Http4s web server framework (brings cats-effect, the IO type, with it)
        "org.http4s" %% "http4s-ember-server" % Versions.Http4s,
        "org.http4s" %% "http4s-dsl" % Versions.Http4s,
        // Logging
        "org.typelevel" %% "log4cats-slf4j" % Versions.Log4Cats,
        "ch.qos.logback" % "logback-classic" % Versions.Logback
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
        "com.raquo" %%% "laminar" % Versions.Laminar
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
    "-language:implicitConversions"
  )
)

lazy val noPublish = Seq(
  publishLocal / skip := true,
  publish / skip := true
)
