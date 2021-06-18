import play.sbt.routes.RoutesKeys
import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings
import sbt.Tests.Group
import sbt.Tests.SubProcess

val appName = "transits-movements-trader-at-departure"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .configs(IntegrationTest)
  .settings(DefaultBuildSettings.integrationTestSettings())
  .settings(SbtDistributablesPlugin.publishingSettings)
  .settings(inConfig(IntegrationTest)(itSettings))
  .settings(inConfig(IntegrationTest)(scalafmtSettings))
  .settings(inConfig(Test)(testSettings))
  .settings(inThisBuild(buildSettings))
  .settings(scoverageSettings)
  .settings(scalacSettings)
  .settings(
    majorVersion := 0,
    scalaVersion := "2.12.13",
    resolvers += Resolver.jcenterRepo,
    PlayKeys.playDefaultPort := 9490,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    javaOptions ++= Seq(
      "-Djdk.xml.maxOccurLimit=10000"
    ),
    // Import models for query string binding in routes file
    RoutesKeys.routesImport ++= Seq(
      "models._",
      "models.Binders._",
      "java.time.OffsetDateTime"
    )
  )

// Settings for the whole build
lazy val buildSettings = Def.settings(
  scalafmtOnCompile := true,
  useSuperShell := false
)

// Scalac options
lazy val scalacSettings = Def.settings(
  // Disable fatal warnings and warnings from discarding values
  scalacOptions ~= {
    opts =>
      opts.filterNot(Set("-Xfatal-warnings", "-Ywarn-value-discard"))
  },
  // Disable dead code warning as it is triggered by Mockito any()
  Test / scalacOptions ~= {
    opts =>
      opts.filterNot(Set("-Ywarn-dead-code"))
  },
  // Disable warnings arising from generated routing code
  scalacOptions += "-Wconf:src=routes/.*:silent"
)

// Scoverage exclusions and minimums
lazy val scoverageSettings = Def.settings(
  parallelExecution in Test := false,
  ScoverageKeys.coverageMinimum := 90.00,
  ScoverageKeys.coverageExcludedFiles := "<empty>;.*javascript.*;.*Routes.*;",
  ScoverageKeys.coverageFailOnMinimum := true,
  ScoverageKeys.coverageHighlighting := true,
  ScoverageKeys.coverageExcludedPackages := Seq(
    """uk\.gov\.hmrc\.BuildInfo*""",
    """.*\.Routes""",
    """.*\.RoutesPrefix""",
    """.*\.Reverse[^.]*""",
    "testonly",
    "testOnly.*",
    "config.*"
  ).mkString(";")
)

lazy val itSettings = Def.settings(
  // Must fork so that config system properties are set
  fork := true,
  unmanagedSourceDirectories += (baseDirectory.value / "test" / "generators"),
  unmanagedResourceDirectories += (baseDirectory.value / "it" / "resources"),
  javaOptions ++= Seq(
    "-Dconfig.resource=it.application.conf",
    "-Dlogger.resource=it.logback.xml"
  )
)

lazy val testSettings = Def.settings(
  // Must fork so that config system properties are set
  fork := true,
  unmanagedResourceDirectories += (baseDirectory.value / "test" / "resources"),
  javaOptions ++= Seq(
    "-Dconfig.resource=test.application.conf"
  )
)
