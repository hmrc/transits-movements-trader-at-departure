import play.sbt.routes.RoutesKeys
import uk.gov.hmrc.SbtArtifactory
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

val appName = "transits-movements-trader-at-departure"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .disablePlugins(JUnitXmlReportPlugin)
  .configs(IntegrationTest)
  .settings(inConfig(IntegrationTest)(itSettings): _*)
  .settings(inConfig(Test)(testSettings): _*)
  .settings(
    majorVersion := 0,
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test,
    useSuperShell in ThisBuild := false
  )
  .settings(publishingSettings: _*)
  .settings(resolvers += Resolver.jcenterRepo)
  .settings(PlayKeys.playDefaultPort := 9490)
  .settings(
    scalacOptions += "-Ypartial-unification"
  )
  .settings(RoutesKeys.routesImport ++= Seq(
    "models._"
  ))

lazy val itSettings = Defaults.itSettings ++ Seq(
  unmanagedSourceDirectories := Seq(
    baseDirectory.value / "it",
    baseDirectory.value / "test" / "generators"
  ),
  unmanagedResourceDirectories := Seq(
    baseDirectory.value / "it" / "resources"
  ),
  parallelExecution := false,
  fork := true,
  javaOptions ++= Seq(
    "-Dconfig.resource=it.application.conf")
)

lazy val testSettings = Seq(
  fork := false,
  javaOptions ++= Seq(
    "-Dconfig.resource=test.application.conf"
  ),
  unmanagedResourceDirectories := Seq(
    baseDirectory.value / "test" / "resources"
  )
)

dependencyOverrides ++= AppDependencies.overrides
