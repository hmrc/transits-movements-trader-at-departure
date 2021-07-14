/*
 * Copyright 2021 HM Revenue & Customs
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
import com.mongodb.MongoClientSettings
import com.mongodb.client.MongoClients
import com.mongodb.connection.ClusterType
import config.AppConfig
import models.Departure
import models.MongoDateTimeFormats
import org.bson.codecs.configuration.CodecRegistries
import play.api.Configuration
import play.api.Logging
import uk.gov.hmrc.mongo.play.json.Codecs

import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.Promise

@Singleton
class MigrationRunner @Inject()(config: Configuration, appConfig: AppConfig) extends Logging {
  lazy val migrationsCompleted = Promise[StandaloneMigrationSuccessEvent]()

  private lazy val mongockRunner = {
    val connectionString = new ConnectionString(config.get[String]("mongodb.uri"))

    val clientSettings = MongoClientSettings
      .builder()
      .applyConnectionString(connectionString)
      .codecRegistry(
        CodecRegistries.fromRegistries(
          CodecRegistries.fromCodecs(
            Codecs.playFormatCodec(MongoDateTimeFormats.localDateTimeFormat),
            Codecs.playFormatCodec(Departure.formatsDeparture)
          ),
          MongoClientSettings.getDefaultCodecRegistry()
        )
      )
      .build()

    val mongoClient = MongoClients.create(clientSettings)

    val mongoDriver = MongoSync4Driver.withDefaultLock(mongoClient, appConfig.appName)

    val clusterType = mongoClient.getClusterDescription.getType
    if (clusterType == ClusterType.STANDALONE || clusterType == ClusterType.UNKNOWN) {
      mongoDriver.disableTransaction()
    }

    mongoDriver.setIndexCreation(true)

    MongockStandalone
      .builder()
      .setDriver(mongoDriver)
      .addChangeLogsScanPackage("migrations.changelogs")
      .setMigrationStartedListener(
        () => logger.info("Started Mongock migrations")
      )
      .setMigrationSuccessListener {
        successEvent =>
          logger.info("Finished Mongock migrations successfully")
          migrationsCompleted.success(successEvent)
      }
      .setMigrationFailureListener {
        failureEvent =>
          val exception = failureEvent.getException
          logger.error("Mongock migrations failed", exception)
          migrationsCompleted.failure(exception)
      }
      .buildRunner()
  }

  mongockRunner.execute()
}
