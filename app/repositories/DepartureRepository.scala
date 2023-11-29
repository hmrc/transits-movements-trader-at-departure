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

import cats.data.Ior
import com.google.inject.ImplementedBy
import com.kenshoo.play.metrics.Metrics
import com.mongodb.client.model.Filters.empty
import com.mongodb.client.model.Updates
import config.AppConfig
import logging.Logging
import metrics.HasMetrics
import models._
import models.response.ResponseDeparture
import models.response.ResponseDepartures
import org.bson.conversions.Bson
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.Sorts.descending
import org.mongodb.scala.model._
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.json.OFormat.oFormatFromReadsAndOWrites
import repositories.DepartureRepositoryImpl.EPOCH_TIME
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.time.Clock
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.Collections
import java.util.concurrent.TimeUnit
import javax.inject.Inject
import javax.inject.Singleton
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

@ImplementedBy(classOf[DepartureRepositoryImpl])
trait DepartureRepository {

  val started: Future[Unit]
  def bulkInsert(departures: Seq[Departure]): Future[Unit]
  def insert(departure: Departure): Future[Unit]
  def addNewMessage(departureId: DepartureId, message: Message): Future[Try[Unit]]
  def setMessageState(departureId: DepartureId, messageId: Int, messageStatus: MessageStatus): Future[Try[Unit]]
  def getMaxDepartureId: Future[Option[DepartureId]]
  def get(departureId: DepartureId): Future[Option[Departure]]
  def get(departureId: DepartureId, channelFilter: ChannelType): Future[Option[Departure]]
  def getWithoutMessages(departureId: DepartureId): Future[Option[DepartureWithoutMessages]]
  def getWithoutMessages(departureId: DepartureId, channelFilter: ChannelType): Future[Option[DepartureWithoutMessages]]
  def getMessagesOfType(departureId: DepartureId, channelFilter: ChannelType, messageTypes: List[MessageType]): Future[Option[DepartureMessages]]
  def getMessage(departureId: DepartureId, channelFilter: ChannelType, messageId: MessageId): Future[Option[Message]]
  def addResponseMessage(departureId: DepartureId, message: Message, lastUpdated: LocalDateTime): Future[Try[Unit]]
  def setMrnAndAddResponseMessage(departureId: DepartureId, message: Message, mrn: MovementReferenceNumber, lastUpdated: LocalDateTime): Future[Try[Unit]]
  def updateDeparture(selector: DepartureId, modifier: MessageStatusUpdate): Future[Try[Unit]]

  def fetchAllDepartures(
    enrolmentId: Ior[TURN, EORINumber],
    channelFilter: ChannelType,
    updatedSince: Option[OffsetDateTime],
    lrn: Option[String] = None,
    pageSize: Option[Int] = None,
    page: Option[Int] = None
  ): Future[ResponseDepartures]
}

object DepartureRepositoryImpl {
  val EPOCH_TIME: LocalDateTime = LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)
  val collectionName            = "departures"
}

@Singleton
class DepartureRepositoryImpl @Inject()(
  mongoComponent: MongoComponent,
  appConfig: AppConfig,
  config: Configuration,
  val metrics: Metrics
)(implicit ec: ExecutionContext, clock: Clock)
    extends PlayMongoRepository[Departure](
      mongoComponent = mongoComponent,
      collectionName = "departures",
      domainFormat = Departure.formatsDeparture,
      indexes = Seq(
        IndexModel(
          Indexes.ascending("eoriNumber"),
          IndexOptions().name("eori-number-index").sparse(false).unique(false).background(false)
        ),
        IndexModel(
          Indexes.ascending("channel"),
          IndexOptions().name("channel-index").sparse(false).unique(false).background(false)
        ),
        IndexModel(
          Indexes.ascending("referenceNumber"),
          IndexOptions().name("reference-number-index").sparse(false).unique(false).background(false)
        ),
        IndexModel(
          Indexes.ascending("lastUpdated"),
          IndexOptions().name("last-updated-index").expireAfter(appConfig.cacheTtl, TimeUnit.SECONDS).unique(false).sparse(false).background(false)
        ),
        IndexModel(
          Indexes.ascending("channel", "eoriNumber"),
          IndexOptions().name("fetch-all-index").sparse(false).unique(false).background(false)
        ),
        IndexModel(
          Indexes.compoundIndex(Indexes.ascending("channel"), Indexes.ascending("eoriNumber"), Indexes.descending("lastUpdated")),
          IndexOptions().name("fetch-all-with-date-filter-index").sparse(false).unique(false).background(false)
        )
      ),
      extraCodecs = Seq(
        Codecs.playFormatCodec(DepartureId.formatsDepartureId),
        Codecs.playFormatCodec(ChannelType.formats),
        Codecs.playFormatCodec(MovementReferenceNumber.mrnFormat),
        Codecs.playFormatCodec(Box.formatsBox),
        Codecs.playFormatCodec(Box.optionBoxFormats),
        Codecs.playFormatCodec(MessageId.formatMessageId),
        Codecs.playFormatCodec(EORINumber.format),
        Codecs.playFormatCodec(Departure.formatsDeparture),
        Codecs.playFormatCodec(Message.format),
        Codecs.playFormatCodec(MessageWithStatus.format),
        Codecs.playFormatCodec(MessageWithoutStatus.format),
        Codecs.playFormatCodec(DepartureWithoutMessages.formatsDeparture),
        Codecs.playFormatCodec(MongoDateTimeFormats.localDateTimeFormat),
        Codecs.playFormatCodec(MongoDateTimeFormats.offsetDateTimeFormat),
        Codecs.playFormatCodec(DepartureMessages.formatsDeparture)
      )
    )
    with DepartureRepository
    with HasMetrics
    with Logging {

  private lazy val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  override val started: Future[Unit] = ensureIndexes.map(
    _ => ()
  )

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

  def getMaxDepartureId: Future[Option[DepartureId]] =
    if (featureFlag) {
      collection.find().sort(descending("_id")).limit(1).headOption().map(_.map(_.departureId))
    } else Future.successful(None)

  def addNewMessage(departureId: DepartureId, message: Message): Future[Try[Unit]] =
    collection
      .updateOne(
        filter = Filters.eq("_id", departureId),
        update =
          Updates.combine(Updates.set("lastUpdated", message.received.get), Updates.inc("nextMessageCorrelationId", 1), Updates.push("messages", message))
      )
      .toFuture()
      .map {
        result =>
          if (result.wasAcknowledged()) {
            if (result.getModifiedCount == 0) Failure(new Exception(s"Could not find departure $departureId"))
            else Success(())
          } else Failure(new Exception("Failed to update departure"))

      }

  def setMessageState(departureId: DepartureId, messageId: Int, messageStatus: MessageStatus): Future[Try[Unit]] = {

    val selector = Filters.and(Filters.eq("_id", departureId), Filters.exists(s"messages.$messageId.status", true))

    val modifier = Updates.combine(Updates.set(s"messages.$messageId.status", messageStatus.toString))
    collection
      .updateOne(filter = selector, update = modifier)
      .toFuture()
      .map {
        result =>
          if (result.wasAcknowledged()) {
            if (result.getModifiedCount == 0) Failure(new Exception("Unable to update message status"))
            else
              Success(())
          } else Failure(new Exception("Unable to update message status"))

      }
  }

  def get(departureId: DepartureId): Future[Option[Departure]] =
    collection
      .find(Filters.eq("_id", departureId))
      .headOption()

  def get(departureId: DepartureId, channelFilter: ChannelType): Future[Option[Departure]] =
    collection
      .find(Filters.and(Filters.eq("_id", departureId), Filters.eq("channel", channelFilter.toString)))
      .headOption()

  def getWithoutMessages(departureId: DepartureId): Future[Option[DepartureWithoutMessages]] = {
    val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))
    val projection    = DepartureWithoutMessages.projection ++ nextMessageId
    val filter        = Aggregates.filter(Filters.eq("_id", departureId))
    val aggregates    = Seq(filter, Aggregates.project(Codecs.toBson(projection).asDocument()))
    collection
      .aggregate[DepartureWithoutMessages](aggregates)
      .allowDiskUse(true)
      .headOption()
      .map(
        opt =>
          opt.map(
            d => d.copy(nextMessageId = MessageId(d.nextMessageId.value + 1))
        )
      )
  }

  def getWithoutMessages(departureId: DepartureId, channelFilter: ChannelType): Future[Option[DepartureWithoutMessages]] = {
    val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

    val projection       = DepartureWithoutMessages.projection ++ nextMessageId
    val filter           = Filters.and(Filters.eq("_id", departureId), Filters.eq("channel", channelFilter.toString))
    val aggregatesFilter = Aggregates.filter(filter)
    val aggregates       = Seq(aggregatesFilter, Aggregates.project(Codecs.toBson(projection).asDocument()))
    collection
      .aggregate[DepartureWithoutMessages](aggregates)
      .allowDiskUse(true)
      .headOption()
      .map(
        opt =>
          opt.map(
            d => d.copy(nextMessageId = MessageId(d.nextMessageId.value + 1))
        )
      )
  }

  def getMessagesOfType(departureId: DepartureId, channelFilter: ChannelType, messageTypes: List[MessageType]): Future[Option[DepartureMessages]] = {
    val filter  = Aggregates.filter(Filters.and(Filters.eq("_id", departureId), Filters.eq("channel", channelFilter.toString)))
    val project = Aggregates.project(Codecs.toBson(DepartureMessages.projection).asDocument())
    val messagesFilter = BsonDocument(
      "$filter" -> Document(
        "input" -> "$messages",
        "as"    -> "message",
        "cond"  -> Document("$in" -> BsonArray("$$message.messageType", messageTypes.map(_.code).toSeq))
      )
    )
    val secondaryFilter = Aggregates.addFields(Field("messages", messagesFilter))

    val aggregates = Seq(filter, project, secondaryFilter)
    collection.aggregate[DepartureMessages](aggregates).allowDiskUse(true).headOption()
  }

  def getMessage(departureId: DepartureId, channelFilter: ChannelType, messageId: MessageId): Future[Option[Message]] = {
    val initialFilter = Aggregates.filter(
      Filters.and(
        Filters.eq("_id", departureId),
        Filters.eq("channel", channelFilter.toString),
        Filters.elemMatch("messages", Filters.eq("messageId", messageId.value))
      )
    )
    val unwindMessages  = Aggregates.unwind("$messages")
    val secondaryFilter = Aggregates.filter(Filters.eq("messages.messageId", messageId.value))
    val replaceRoot     = Aggregates.replaceRoot("$messages")
    val aggregates      = Seq(initialFilter, unwindMessages, secondaryFilter, replaceRoot)
    collection.aggregate[Message](aggregates).allowDiskUse(true).headOption()
  }

  def addResponseMessage(departureId: DepartureId, message: Message, lastUpdated: LocalDateTime): Future[Try[Unit]] = {
    val selector = Filters.eq("_id", departureId)
    val modifier = Updates.combine(Updates.set("lastUpdated", lastUpdated), Updates.push("messages", message))
    collection
      .updateOne(filter = selector, update = modifier)
      .toFuture()
      .map {
        result =>
          if (result.wasAcknowledged()) {
            if (result.getModifiedCount == 0) Failure(new Exception(s"Could not find departure $departureId"))
            else Success(())
          } else Failure(new Exception("Failed to update departure"))

      }
  }

  def setMrnAndAddResponseMessage(departureId: DepartureId, message: Message, mrn: MovementReferenceNumber, lastUpdated: LocalDateTime): Future[Try[Unit]] = {

    val selector = Filters.eq("_id", departureId)
    val modifier =
      Updates.combine(Updates.set("lastUpdated", lastUpdated), Updates.set("movementReferenceNumber", mrn), Updates.push("messages", message))
    collection
      .updateOne(filter = selector, update = modifier)
      .toFuture()
      .map {
        result =>
          if (result.wasAcknowledged()) {
            if (result.getModifiedCount == 0) Failure(new Exception(s"Could not find departure $departureId"))
            else Success(())
          } else Failure(new Exception("Failed to update departure"))

      }
  }

  private def lrnFilter(movementReferenceNumber: Option[String]): Bson =
    movementReferenceNumber match {
      case Some(movementReferenceNumber) => Filters.regex("referenceNumber", s"\\Q$movementReferenceNumber\\E", "i")
      case _                             => empty()
    }

  def fetchAllDepartures(
    enrolmentId: Ior[TURN, EORINumber],
    channelFilter: ChannelType,
    updatedSince: Option[OffsetDateTime],
    lrn: Option[String] = None,
    pageSize: Option[Int] = None,
    page: Option[Int] = None
  ): Future[ResponseDepartures] = withMetricsTimerAsync("mongo-get-departures-for-eori") {
    _ =>
      val enrolmentIds = enrolmentId.fold(
        turn => List(turn.value),
        eoriNumber => List(eoriNumber.value),
        (turn, eoriNumber) => List(eoriNumber.value, turn.value)
      )

      val baseSelector = Filters.and(Filters.in("eoriNumber", enrolmentIds.map(_.toString): _*), Filters.eq("channel", channelFilter.toString))

      val dateTimeFilter: Bson =
        Filters.gte("lastUpdated", updatedSince.map(_.toLocalDateTime).getOrElse(EPOCH_TIME))

      val fullSelector =
        Filters.and(Filters.in("eoriNumber", enrolmentIds.map(_.toString): _*), Filters.eq("channel", channelFilter.toString), lrnFilter(lrn), dateTimeFilter)

      val nextMessageId = Json.obj("nextMessageId" -> Json.obj("$size" -> "$messages"))

      val projection = DepartureWithoutMessages.projection ++ nextMessageId
      val limit      = pageSize.map(Math.max(1, _)).getOrElse(appConfig.maxRowsReturned(channelFilter))

      val skip            = Math.abs(page.getOrElse(1) - 1) * limit
      val fetchCount      = collection.countDocuments(baseSelector).toFuture().map(_.toInt)
      val fetchMatchCount = collection.countDocuments(fullSelector).toFuture().map(_.toInt)
      val matchStage      = Aggregates.filter(fullSelector)
      val projectStage    = Aggregates.project(Codecs.toBson(projection).asDocument())
      val sortStage       = Aggregates.sort(descending("lastUpdated"))
      val skipStage       = Aggregates.skip(skip)
      val limitStage      = Aggregates.limit(limit)
      val restStages =
        if (skip > 0)
          Seq(matchStage) ++ Seq(projectStage, sortStage, skipStage, limitStage)
        else
          Seq(matchStage) ++ Seq(projectStage, sortStage, limitStage)
      val fetchResults = collection
        .aggregate[DepartureWithoutMessages](restStages)
        .allowDiskUse(true)
        .toFuture()
        .map {
          response =>
            response.map(ResponseDeparture.fromDepartureWithoutMessage)
        }
      for {
        fetchResults    <- fetchResults
        fetchCount      <- fetchCount
        fetchMatchCount <- fetchMatchCount
      } yield ResponseDepartures(fetchResults, fetchResults.length, fetchCount, fetchMatchCount)
  }

  def updateDeparture(selector: DepartureId, modifier: MessageStatusUpdate): Future[Try[Unit]] = {
    val filter     = Filters.eq("_id", selector)
    val setUpdated = Updates.set("lastUpdated", LocalDateTime.now(clock))

    val arrayFilters = new UpdateOptions().arrayFilters(Collections.singletonList(Filters.in("element.messageId", modifier.messageId.value)))
    val setStatus    = Updates.set("messages.$[element].status", modifier.messageStatus.toString)
    collection.updateOne(filter = filter, update = Updates.combine(setStatus, setUpdated), options = arrayFilters).toFuture().map {
      result =>
        if (result.wasAcknowledged()) {
          if (result.getModifiedCount == 0) Failure(new Exception("Unable to update message status"))
          else Success(())
        } else Failure(new Exception("Unable to update message status"))

    }
  }
}
