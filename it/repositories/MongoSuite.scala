/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package repositories

import com.typesafe.config.ConfigFactory
import migrations.MigrationRunnerImpl
import org.scalatest._
import org.scalatest.concurrent._
import org.scalatest.time.{Seconds, Span}
import play.api.{Application, Configuration}
import reactivemongo.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MongoSuite {

  private lazy val config = Configuration(ConfigFactory.load(System.getProperty(
    "config.resource"
  )))

  private lazy val parsedUri =  
    MongoConnection.fromString(config.get[String]("mongodb.uri"))
  
  lazy val connection = parsedUri.flatMap {AsyncDriver().connect(_)}
}

trait MongoSuite extends ScalaFutures {
  self: TestSuite =>

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(timeout = Span(30, Seconds))

  def started(app: Application): Future[Unit] = {

    val departureRepository = app.injector.instanceOf[DepartureRepository]
    val lockRepository = app.injector.instanceOf[LockRepository]
    val migrationRunner = app.injector.instanceOf[MigrationRunnerImpl]

    val services = Seq(departureRepository.started, lockRepository.started, migrationRunner.migrationsCompleted)

    Future.sequence(services).map(_ => ())
  }

  def database: Future[DefaultDB] = {
    for {
      uri <-MongoSuite.parsedUri
      connection <- MongoSuite.connection
      database <- connection.database(uri.db.get)
    } yield database
  }

}