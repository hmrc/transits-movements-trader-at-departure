import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  private val catsVersion      = "2.7.0"
  private val mongockVersion   = "4.3.8"
  private val bootstrapVersion = "7.23.0"

  val compile = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-play-28"        % "0.74.0",
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-28" % bootstrapVersion,
    "org.typelevel"                %% "cats-core"                 % catsVersion,
    "org.json"                      % "json"                      % "20230227",
    "com.github.cloudyrock.mongock" % "mongock-standalone"        % mongockVersion,
    "com.github.cloudyrock.mongock" % "mongodb-sync-v4-driver"    % mongockVersion,
    "org.mongodb"                   % "mongodb-driver-sync"       % "4.6.0"
  )

  val test = Seq(
    "org.mockito"             % "mockito-core"               % "3.12.4",
    "org.scalatest"          %% "scalatest"                  % "3.2.10",
    "com.typesafe.play"      %% "play-test"                  % current,
    "org.scalatestplus.play" %% "scalatestplus-play"         % "5.1.0",
    "org.scalatestplus"      %% "mockito-3-2"                % "3.1.2.0",
    "org.scalacheck"         %% "scalacheck"                 % "1.15.4",
    "com.github.tomakehurst"  % "wiremock-standalone"        % "2.27.2",
    "org.typelevel"          %% "cats-laws"                  % catsVersion,
    "org.typelevel"          %% "discipline-core"            % "1.1.5",
    "org.typelevel"          %% "discipline-scalatest"       % "2.1.5",
    "com.vladsch.flexmark"    % "flexmark-all"               % "0.64.6",
    "com.typesafe.akka"      %% "akka-testkit"               % "2.6.21",
    "com.typesafe.akka"      %% "akka-stream-testkit"        % "2.6.21",
    "com.typesafe.akka"      %% "akka-actor-typed"           % "2.6.21",
    "com.typesafe.akka"      %% "akka-serialization-jackson" % "2.6.21",
    "com.typesafe.akka"      %% "akka-slf4j"                 % "2.6.21",
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-28"    % "0.74.0",
    "uk.gov.hmrc"            %% "bootstrap-test-play-28"     % bootstrapVersion
  ).map(_ % "test, it")
}
