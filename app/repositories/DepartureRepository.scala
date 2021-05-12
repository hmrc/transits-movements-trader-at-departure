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

package repositories

import java.time.Clock
import java.time.OffsetDateTime
import config.AppConfig
import javax.inject.Inject
import models._
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.bson.collection.BSONSerializationPack
import reactivemongo.api.Cursor
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.indexes.Index.Aux
import reactivemongo.api.indexes.IndexType
import reactivemongo.bson.BSONDocument
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.IndexUtils

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import scala.util.Try

class DepartureRepository @Inject()(mongo: ReactiveMongoApi, appConfig: AppConfig)(implicit ec: ExecutionContext, clock: Clock) extends MongoDateTimeFormats {

  private lazy val eoriNumberIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("eoriNumber" -> IndexType.Ascending),
    name = Some("eori-number-index")
  )

  private lazy val channelIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("channelType" -> IndexType.Ascending),
    name = Some("channel-type-index")
  )

  private lazy val referenceNumberIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("referenceNumber" -> IndexType.Ascending),
    name = Some("reference-number-index")
  )

  private lazy val lastUpdatedIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("lastUpdated" -> IndexType.Ascending),
    name = Some("last-updated-index"),
    options = BSONDocument("expireAfterSeconds" -> appConfig.cacheTtl)
  )

  val started: Future[Unit] = {
    collection
      .flatMap {
        jsonCollection =>
          for {
            _   <- jsonCollection.indexesManager.ensure(channelIndex)
            _   <- jsonCollection.indexesManager.ensure(eoriNumberIndex)
            _   <- jsonCollection.indexesManager.ensure(referenceNumberIndex)
            res <- jsonCollection.indexesManager.ensure(lastUpdatedIndex)
          } yield res
      }
      .map(_ => ())
  }

  private val collectionName = DepartureRepository.collectionName

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def insert(departure: Departure): Future[Unit] =
    collection.flatMap {
      _.insert(false)
        .one(Json.toJsObject(departure))
        .map(_ => ())
    }

  def addNewMessage(departureId: DepartureId, message: Message): Future[Try[Unit]] = {

    val selector = Json.obj(
      "_id" -> departureId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated" -> message.dateTime
        ),
        "$inc" -> Json.obj(
          "nextMessageCorrelationId" -> 1
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map {
          _.lastError
            .map {
              le =>
                if (le.updatedExisting) Success(()) else Failure(new Exception(s"Could not find departure $departureId"))
            }
            .getOrElse(Failure(new Exception("Failed to update departure")))
        }
    }
  }

  @deprecated("Use updateDeparture since this will be removed in the next version", "next")
  def setDepartureStateAndMessageState(departureId: DepartureId,
                                       messageId: MessageId,
                                       departureState: DepartureStatus,
                                       messageState: MessageStatus): Future[Option[Unit]] = {
    val selector = DepartureIdSelector(departureId)

    val modifier = CompoundStatusUpdate(DepartureStatusUpdate(departureState), MessageStatusUpdate(messageId, messageState))

    updateDeparture(selector, modifier).map(_.toOption)
  }

  def setMessageState(departureId: DepartureId, messageId: Int, messageStatus: MessageStatus): Future[Try[Unit]] = {
    val selector = Json.obj(
      "$and" -> Json.arr(
        Json.obj("_id"                         -> departureId),
        Json.obj(s"messages.$messageId.status" -> Json.obj("$exists" -> true))
      )
    )

    val modifier = Json.obj(
      "$set" -> Json.obj(
        s"messages.$messageId.status" -> messageStatus.toString
      )
    )

    collection.flatMap {
      _.update(false)
        .one(selector, modifier)
        .map {
          WriteResult
            .lastError(_)
            .map {
              le =>
                if (le.updatedExisting) Success(())
                else
                  Failure(new Exception(le.errmsg match {
                    case Some(err) => err
                    case None      => "Unable to update message status"
                  }))
            }
            .getOrElse(Failure(new Exception("Unable to update message status")))
        }
    }
  }

  def get(departureId: DepartureId): Future[Option[Departure]] = {

    val selector = Json.obj(
      "_id" -> departureId
    )

    collection.flatMap {
      _.find(selector, None)
        .one[Departure]
    }
  }

  def get(departureId: DepartureId, channelFilter: ChannelType): Future[Option[Departure]] = {

    val selector = Json.obj(
      "_id"     -> departureId,
      "channel" -> channelFilter
    )

    collection.flatMap {
      _.find(selector, None)
        .one[Departure]
    }
  }

  def addResponseMessage(departureId: DepartureId, message: Message, status: DepartureStatus): Future[Try[Unit]] = {
    val selector = Json.obj(
      "_id" -> departureId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated" -> message.dateTime,
          "status"  -> status.toString
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map {
          _.lastError
            .map {
              le =>
                if (le.updatedExisting) Success(()) else Failure(new Exception(s"Could not find departure $departureId"))
            }
            .getOrElse(Failure(new Exception("Failed to update departure")))
        }
    }
  }

  def setMrnAndAddResponseMessage(departureId: DepartureId, message: Message, status: DepartureStatus, mrn: MovementReferenceNumber): Future[Try[Unit]] = {
    val selector = Json.obj(
      "_id" -> departureId
    )

    val modifier =
      Json.obj(
        "$set" -> Json.obj(
          "updated"                 -> message.dateTime,
          "movementReferenceNumber" -> mrn,
          "status"                  -> status.toString
        ),
        "$push" -> Json.obj(
          "messages" -> Json.toJson(message)
        )
      )

    collection.flatMap {
      _.findAndUpdate(selector, modifier)
        .map {
          _.lastError
            .map {
              le =>
                if (le.updatedExisting) Success(()) else Failure(new Exception(s"Could not find departure $departureId"))
            }
            .getOrElse(Failure(new Exception("Failed to update departure")))
        }
    }
  }

  def fetchAllDepartures(eoriNumber: String, channelFilter: ChannelType, updatedSince: Option[OffsetDateTime]): Future[Seq[DepartureWithoutMessages]] = {
    val dateFilter = updatedSince.map(dateTime => Json.obj("updated" -> Json.obj("$gte" -> dateTime))).getOrElse(Json.obj())
    val selector   = Json.obj("eoriNumber" -> eoriNumber, "channel" -> channelFilter) ++ dateFilter

    collection.flatMap {
      _.find(selector, DepartureWithoutMessages.projection)
        .sort(Json.obj("lastUpdated" -> -1))
        .cursor[DepartureWithoutMessages]()
        .collect[Seq](appConfig.maxRowsReturned(channelFilter), Cursor.FailOnError())
    }
  }

  def updateDeparture[A](selector: DepartureSelector, modifier: A)(implicit ev: DepartureModifier[A]): Future[Try[Unit]] = {

    import models.DepartureModifier.toJson

    collection.flatMap {
      _.update(false)
        .one[JsObject, JsObject](Json.toJsObject(selector), modifier)
        .map {
          writeResult =>
            if (writeResult.n > 0)
              Success(())
            else
              writeResult.errmsg
                .map(x => Failure(new Exception(x)))
                .getOrElse(Failure(new Exception("Unable to update message status")))
        }
    }
  }
}

object DepartureRepository {
  val collectionName = "departures"
}
