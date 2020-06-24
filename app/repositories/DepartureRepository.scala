/*
 * Copyright 2020 HM Revenue & Customs
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

import javax.inject.Inject
import models.{Departure, DepartureId, DepartureStatus, Message, MessageStatus, MongoDateTimeFormats}
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.play.json.collection.JSONCollection
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class DepartureRepository @Inject()(mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) extends MongoDateTimeFormats {

  private val index = Index(
    key = Seq("eoriNumber" -> IndexType.Ascending),
    name = Some("eori-number-index")
  )

  val started: Future[Unit] = {
    collection
      .flatMap {
        _.indexesManager.ensure(index)
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
            .getOrElse(Failure(new Exception("Failed to update arrival")))
        }
    }
  }

  def setDepartureStateAndMessageState(departureId: DepartureId, messageId: Int, departureStatus: DepartureStatus, messageStatus: MessageStatus): Future[Option[Unit]] = {

    val selector = Json.obj("_id" -> departureId)

    val modifier = Json.obj(
      "$set" -> Json.obj(
        s"messages.${messageId}.status" -> messageStatus.toString,
        "status"                              -> departureStatus.toString
      )
    )

    collection.flatMap {
      _.update(false)
        .one(selector, modifier)
        .map {
          y =>
            if (y.n == 1) Some(())
            else None
        }
    }
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

  def get(eoriNumber: String, reference: String): Future[Option[Departure]] = {
    val selector = Json.obj(
      "referenceNumber" -> reference,
      "eoriNumber"              -> eoriNumber
    )

    collection.flatMap {
      _.find(selector, None)
        .one[Departure]
    }
  }
}

object DepartureRepository {
  val collectionName = "departures"
}