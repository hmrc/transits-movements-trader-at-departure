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

import javax.inject.Inject
import models.DepartureId
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

class DepartureIdRepository @Inject()(mongo: ReactiveMongoApi) {

  private val lastIndexKey = "last-index"
  private val primaryValue = "record_id"

  private val collectionName: String = DepartureIdRepository.collectionName

  private val indexKeyReads: Reads[DepartureId] = {
    import play.api.libs.json._
    (__ \ lastIndexKey).read[DepartureId]
  }

  private def collection: Future[JSONCollection] =
    mongo.database.map(_.collection[JSONCollection](collectionName))

  def nextId(): Future[DepartureId] = {

    val update = Json.obj(
      "$inc" -> Json.obj(lastIndexKey -> 1)
    )

    val selector = Json.obj("_id" -> primaryValue)

    collection.flatMap(
      _.findAndUpdate(selector, update, upsert = true, fetchNewObject = true)
        .map(
          x =>
            x.result(indexKeyReads)
              .getOrElse(throw new Exception(s"Unable to generate DepartureId")))
    )
  }
}

object DepartureIdRepository {
  val collectionName = "departure-ids"
}
