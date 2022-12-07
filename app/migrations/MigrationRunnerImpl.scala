/*
 * Copyright 2023 HM Revenue & Customs
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

package migrations

import com.github.cloudyrock.mongock.driver.mongodb.sync.v4.driver.MongoSync4Driver
import com.github.cloudyrock.standalone.MongockStandalone
import com.github.cloudyrock.standalone.event.StandaloneMigrationSuccessEvent
import com.mongodb.ConnectionString
import com.mongodb.client.MongoClients
import com.mongodb.connection.ClusterType
import play.api.Configuration
import play.api.Logging

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise

trait MigrationRunner

@Singleton
class MigrationRunnerImpl @Inject()(config: Configuration)(implicit ec: ExecutionContext) extends MigrationRunner with Logging {

  private lazy val migrationsCompletedPromise = Promise[StandaloneMigrationSuccessEvent]()
  lazy val migrationsCompleted                = migrationsCompletedPromise.future

  def makeMongockRunner(completionPromise: Promise[StandaloneMigrationSuccessEvent]) = {
    val mongoDbUri  = new ConnectionString(config.get[String]("mongodb.uri"))
    val mongoClient = MongoClients.create(mongoDbUri)
    val mongoDriver = MongoSync4Driver.withDefaultLock(mongoClient, mongoDbUri.getDatabase())

    val clusterType = mongoClient.getClusterDescription.getType
    if (clusterType == ClusterType.STANDALONE || clusterType == ClusterType.UNKNOWN) {
      mongoDriver.disableTransaction()
    }

    mongoDriver.setIndexCreation(true)

    MongockStandalone
      .builder()
      .setDriver(mongoDriver)
      .addChangeLogsScanPackage("migrations.changelogs")
      .dontFailIfCannotAcquireLock()
      .setMigrationStartedListener(
        () => logger.info("Started Mongock migrations")
      )
      .setMigrationSuccessListener {
        successEvent =>
          logger.info("Finished Mongock migrations successfully")
          completionPromise.success(successEvent)
      }
      .setMigrationFailureListener {
        failureEvent =>
          val exception = failureEvent.getException
          logger.error("Mongock migrations failed", exception)
          completionPromise.failure(exception)
      }
      .buildRunner()
  }

  def runMigrations(): Future[StandaloneMigrationSuccessEvent] =
    Future {
      val completionPromise = Promise[StandaloneMigrationSuccessEvent]()
      makeMongockRunner(completionPromise).execute()
      completionPromise.future
    }.flatten

  makeMongockRunner(migrationsCompletedPromise).execute()
}
