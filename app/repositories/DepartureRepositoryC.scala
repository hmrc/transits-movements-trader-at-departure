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

package repositories

import com.google.inject.ImplementedBy
import com.kenshoo.play.metrics.Metrics
import com.mongodb.client.model.Updates
import config.AppConfig
import models._
import org.mongodb.scala.model.Sorts.descending
import org.mongodb.scala.model._
import play.api.Configuration
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.time.Clock
import java.util.concurrent.TimeUnit
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DepartureRepositoryImpl])
trait DepartureRepository {
  def bulkInsert(departures: Seq[Departure]): Future[Unit]
  def insert(departure: Departure): Future[Unit]
  def addNewMessage(departureId: DepartureId, message: Message): Future[Unit]
  def setMessageState(departureId: DepartureId, messageId: Int, messageStatus: MessageStatus): Future[Unit]
  def getMaxDepartureId: Future[Option[DepartureId]]
  def get(departureId: DepartureId): Future[Option[Departure]]
  def get(departureId: DepartureId, channelFilter: ChannelType): Future[Option[Departure]]
  def getWithoutMessages(departureId: DepartureId): Future[Option[DepartureWithoutMessages]]

}

@Singleton
class DepartureRepositoryImpl @Inject() (
  mongoComponent: MongoComponent,
  appConfig: AppConfig,
  config: Configuration,
  val metrics: Metrics
)(implicit ec: ExecutionContext, clock: Clock)
    extends PlayMongoRepository[Departure](
      mongoComponent = mongoComponent,
      collectionName = "departuresNew",
      domainFormat = Departure.formatsDeparture,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("eoriNumber"),
          IndexOptions().name("eori-number-index")
        ),
        IndexModel(
          Indexes.ascending("channel"),
          IndexOptions().name("channel-index")
        ),
        IndexModel(
          Indexes.ascending("referenceNumber"),
          IndexOptions().name("reference-number-index")
        ),
        IndexModel(
          Indexes.ascending("lastUpdated"),
          IndexOptions().name("last-updated-index").expireAfter(appConfig.cacheTtl, TimeUnit.SECONDS).unique(false).sparse(false).background(false)
        )
      )
    )
    with DepartureRepository {
  private lazy val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  def bulkInsert(departures: Seq[Departure]): Future[Unit] = {
    val insertModels = departures.map(
      departure => InsertOneModel(departure)
    )
    collection
      .bulkWrite(insertModels, BulkWriteOptions().ordered(false))
      .toFuture()
      .map(
        _ => ()
      )
  }

  def insert(departure: Departure): Future[Unit] =
    collection
      .insertOne(departure)
      .toFuture()
      .map(
        _ => ()
      )

  def getMaxDepartureId: Future[Option[DepartureId]] = if (featureFlag) {
    collection.find().sort(descending("_id")).limit(1).headOption().map(_.map(_.departureId))
  } else Future.successful(None)

  def addNewMessage(departureId: DepartureId, message: Message): Future[Unit] =
    collection
      .findOneAndUpdate(
        filter = Filters.eq("_id", departureId),
        update =
          Updates.combine(Updates.set("lastUpdated", message.received.get), Updates.inc("nextMessageCorrelationId", 1), Updates.push("messages", message)),
        options = FindOneAndUpdateOptions().upsert(false).bypassDocumentValidation(false)
      )
      .toFuture()
      .map(
        _ => ()
      )

  def setMessageState(departureId: DepartureId, messageId: Int, messageStatus: MessageStatus): Future[Unit] = {

    val selector = Filters.and(Filters.eq("_id", departureId), Filters.exists(s"messages.$messageId.status", true))

    val modifier = Updates.combine(Updates.set(s"messages.$messageId.status", messageStatus.toString))
    collection
      .findOneAndUpdate(filter = selector, update = modifier, options = FindOneAndUpdateOptions().upsert(false))
      .toFuture()
      .map(
        _ => ()
      )
  }

  def get(departureId: DepartureId): Future[Option[Departure]] =
    collection
      .find(Filters.eq("_id", departureId))
      .headOption()

  def get(departureId: DepartureId, channelFilter: ChannelType): Future[Option[Departure]] =
    collection
      .find(Filters.and(Filters.eq("_id", departureId), Filters.eq("channel", channelFilter)))
      .headOption()
  def getWithoutMessages(departureId: DepartureId): Future[Option[DepartureWithoutMessages]]={

  }
}
