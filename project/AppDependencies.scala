import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion    = "2.6.1"
  private val mongockVersion = "4.3.8"

  val compile = Seq(
    "org.reactivemongo"            %% "play2-reactivemongo"             % "0.20.13-play27",
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-28"       % "5.16.0",
    "com.typesafe.play"            %% "play-iteratees"                  % "2.6.1",
    "com.typesafe.play"            %% "play-iteratees-reactive-streams" % "2.6.1",
    "org.typelevel"                %% "cats-core"                       % catsVersion,
    "org.json"                      % "json"                            % "20210307",
    "com.github.cloudyrock.mongock" % "mongock-standalone"              % mongockVersion,
    "com.github.cloudyrock.mongock" % "mongodb-sync-v4-driver"          % mongockVersion,
    "org.mongodb"                   % "mongodb-driver-sync"             % "4.3.1",
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-play-28"              % "0.52.0"
  )

  val test = Seq(
    "org.mockito"             % "mockito-core"         % "3.8.0",
    "org.scalatest"          %% "scalatest"            % "3.2.10",
    "com.typesafe.play"      %% "play-test"            % current,
    "org.scalatestplus.play" %% "scalatestplus-play"   % "4.0.3",
    "org.scalatestplus"      %% "mockito-3-2"          % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"           % "1.15.4",
    "com.github.tomakehurst"  % "wiremock-standalone"  % "2.27.2",
    "org.typelevel"          %% "cats-laws"            % catsVersion,
    "org.typelevel"          %% "discipline-core"      % "1.1.5",
    "org.typelevel"          %% "discipline-scalatest" % "2.1.5",
    "com.vladsch.flexmark"    % "flexmark-all"         % "0.62.2",
    "com.typesafe.akka"      %% "akka-stream-testkit"  % "2.6.14"
  ).map(_ % "test, it")
}
