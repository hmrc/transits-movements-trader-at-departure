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

import java.time.LocalDateTime

import config.AppConfig
import javax.inject.Inject
import models.DepartureId
import models.MongoDateTimeFormats._
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.bson.collection.BSONSerializationPack
import reactivemongo.api.commands.LastError
import reactivemongo.api.indexes.Index.Aux
import reactivemongo.api.indexes.IndexType
import reactivemongo.bson.BSONDocument
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection
import utils.IndexUtils

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class LockRepository @Inject()(mongo: ReactiveMongoApi, appConfig: AppConfig)(implicit ec: ExecutionContext) {

  private val documentExistsErrorCodeValue = 11000

  private val ttl = appConfig.lockRepositoryTtl

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](LockRepository.collectionName))

  private val createdIndex: Aux[BSONSerializationPack.type] = IndexUtils.index(
    key = Seq("created" -> IndexType.Ascending),
    name = Some("created-index"),
    options = BSONDocument("expireAfterSeconds" -> ttl)
  )

  val started: Future[Unit] =
    collection
      .flatMap {
        _.indexesManager.ensure(createdIndex)
      }
      .map(_ => ())

  def lock(departureId: DepartureId): Future[Boolean] = {

    val lock = Json.obj(
      "_id"     -> departureId,
      "created" -> LocalDateTime.now
    )

    collection.flatMap {
      _.insert(ordered = false)
        .one(lock)
        .map(_ => true)
    } recover {
      case e: LastError if e.code.contains(documentExistsErrorCodeValue) =>
        false
    }
  }

  def unlock(departureId: DepartureId): Future[Unit] =
    collection.flatMap {
      _.findAndRemove(Json.obj("_id" -> departureId))
        .map(_ => ())
    }
}

object LockRepository {

  val collectionName = "locks"
}
