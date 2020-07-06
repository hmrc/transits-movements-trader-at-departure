import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion = "2.0.0"

  val compile = Seq(
    "org.reactivemongo" %% "play2-reactivemongo" % "0.18.6-play26",
    "uk.gov.hmrc"       %% "bootstrap-play-26"   % "1.4.0",
    "org.typelevel"     %% "cats-core"           % catsVersion
  )

  val test = Seq(
    "org.mockito"            % "mockito-core"          % "3.3.3",
    "org.scalatest"          %% "scalatest"            % "3.1.2",
    "com.typesafe.play"      %% "play-test"            % current,
    "org.pegdown"            % "pegdown"               % "1.6.0",
    "org.scalatestplus.play" %% "scalatestplus-play"   % "3.1.2",
    "org.scalatestplus"      %% "mockito-3-2"          % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"           % "1.14.1",
    "com.github.tomakehurst" % "wiremock-standalone"   % "2.17.0",
    "org.typelevel"          %% "cats-laws"            % catsVersion,
    "org.typelevel"          %% "discipline-core"      % "1.0.0",
    "org.typelevel"          %% "discipline-scalatest" % "1.0.1",
    "com.vladsch.flexmark"   % "flexmark-all"          % "0.35.10"
  ).map(_ % "test, it")

  val akkaVersion     = "2.5.23"
  val akkaHttpVersion = "10.0.15"

  val overrides = Seq(
    "com.typesafe.akka" %% "akka-stream"    % akkaVersion,
    "com.typesafe.akka" %% "akka-protobuf"  % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j"     % akkaVersion,
    "com.typesafe.akka" %% "akka-actor"     % akkaVersion,
    "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion
  )
}
