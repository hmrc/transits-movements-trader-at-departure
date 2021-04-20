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
    scalaVersion := "2.12.11",
    resolvers += Resolver.jcenterRepo,
    PlayKeys.playDefaultPort := 9490,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    // Import models by default in route files
    RoutesKeys.routesImport ++= Seq(
      "models._"
    ),
    javaOptions ++= Seq(
      "-Djdk.xml.maxOccurLimit=10000"
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
  }
  // Cannot be enabled yet - requires Scala 2.12.13 which suffers from https://github.com/scoverage/scalac-scoverage-plugin/issues/305
  // Disable warnings arising from generated routing code
  // scalacOptions += "-Wconf:src=routes/.*:silent",
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
  ),
  // sbt-settings does not cause javaOptions to be passed to test groups by default
  // needed unless / until this PR is merged and released: https://github.com/hmrc/sbt-settings/pull/19/files
  testGrouping := {
    val tests          = (IntegrationTest / definedTests).value
    val forkJvmOptions = (IntegrationTest / javaOptions).value
    tests.map {
      test =>
        Group(
          test.name,
          Seq(test),
          SubProcess(
            ForkOptions().withRunJVMOptions(forkJvmOptions.toVector :+ ("-Dtest.name=" + test.name))
          )
        )
    }
  }
)

lazy val testSettings = Def.settings(
  // Must fork so that config system properties are set
  fork := true,
  unmanagedResourceDirectories += (baseDirectory.value / "test" / "resources"),
  javaOptions ++= Seq(
    "-Dconfig.resource=test.application.conf"
  )
)
