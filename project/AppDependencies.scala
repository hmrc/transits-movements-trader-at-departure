import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion = "2.5.0"

  val compile = Seq(
    "org.reactivemongo" %% "play2-reactivemongo"             % "0.20.13-play27",
    "uk.gov.hmrc"       %% "bootstrap-backend-play-27"       % "3.3.0",
    "com.typesafe.play" %% "play-iteratees"                  % "2.6.1",
    "com.typesafe.play" %% "play-iteratees-reactive-streams" % "2.6.1",
    "org.typelevel"     %% "cats-core"                       % catsVersion,
    "org.json"          % "json"                             % "20200518"
  )

  val test = Seq(
    "org.mockito"            % "mockito-core"          % "3.8.0",
    "org.scalatest"          %% "scalatest"            % "3.2.5",
    "com.typesafe.play"      %% "play-test"            % current,
    "org.scalatestplus.play" %% "scalatestplus-play"   % "4.0.3",
    "org.scalatestplus"      %% "mockito-3-2"          % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"           % "1.15.3",
    "com.github.tomakehurst" % "wiremock-standalone"   % "2.27.2",
    "org.typelevel"          %% "cats-laws"            % catsVersion,
    "org.typelevel"          %% "discipline-core"      % "1.1.4",
    "org.typelevel"          %% "discipline-scalatest" % "2.1.2",
    "com.vladsch.flexmark"    % "flexmark-all"           % "0.36.8"
  ).map(_ % "test, it")
}
