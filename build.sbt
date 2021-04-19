import uk.gov.hmrc.DefaultBuildSettings
import play.sbt.routes.RoutesKeys
import scoverage.ScoverageKeys
import uk.gov.hmrc.SbtArtifactory
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

val appName = "transits-movements-trader-at-departure"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .configs(IntegrationTest)
  .settings(DefaultBuildSettings.integrationTestSettings(): _*)
  .settings(inConfig(IntegrationTest)(itSettings): _*)
  .settings(inConfig(IntegrationTest)(ScalafmtPlugin.scalafmtConfigSettings): _*)
  .settings(inConfig(Test)(testSettings): _*)
  .settings(scalaVersion := "2.12.13")
  .settings(
    majorVersion := 0,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    // Disable fatal warnings and warnings from discarding values
    scalacOptions ~= { opts => opts.filterNot(Set("-Xfatal-warnings", "-Ywarn-value-discard")) },
    // Disable warnings arising from generated routing code
    scalacOptions += "-Wconf:src=routes/.*:silent",
    // Disable dead code warning as it is triggered by Mockito any()
    Test / scalacOptions ~= { opts => opts.filterNot(Set("-Ywarn-dead-code")) },
    useSuperShell in ThisBuild := false,
    javaOptions ++= Seq(
      "-Djdk.xml.maxOccurLimit=10000"
    )
  )
  .settings(publishingSettings: _*)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(PlayKeys.playDefaultPort := 9490)
  .settings(scoverageSettings: _*)
  .settings(RoutesKeys.routesImport ++= Seq(
    "models._"
  ))

lazy val scoverageSettings = {
  Seq(
    ScoverageKeys.coverageExcludedPackages := """uk\.gov\.hmrc\.BuildInfo*;.*\.Routes;.*\.RoutesPrefix;.*\.Reverse[^.]*;testonly;config.*""",
    ScoverageKeys.coverageMinimum := 90.00,
    ScoverageKeys.coverageExcludedFiles := "<empty>;.*javascript.*;.*Routes.*;",
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

lazy val itSettings = Defaults.itSettings ++ Seq(
  unmanagedSourceDirectories := Seq(
    baseDirectory.value / "it",
    baseDirectory.value / "test" / "generators"
  ),
  unmanagedResourceDirectories := Seq(
    baseDirectory.value / "it" / "resources"
  ),
  javaOptions ++= Seq(
    "-Dconfig.resource=it.application.conf",
    "-Dlogger.resource=it.logback.xml"
  )
)

lazy val testSettings = Seq(
  fork := true,
  javaOptions ++= Seq(
    "-Dconfig.resource=test.application.conf"
  ),
  unmanagedResourceDirectories := Seq(
    baseDirectory.value / "test" / "resources"
  )
)
