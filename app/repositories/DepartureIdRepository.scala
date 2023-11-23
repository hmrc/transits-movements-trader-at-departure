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
import com.mongodb.client.model.Filters
import com.mongodb.client.model.Updates
import models.DepartureId
import models.DepartureIdWrapper
import org.mongodb.scala.model.FindOneAndUpdateOptions
import org.mongodb.scala.model.UpdateOptions
import play.api.Configuration
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.Codecs
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import javax.inject.Inject
import javax.inject.Singleton
import scala.collection.immutable.Seq
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

@ImplementedBy(classOf[DepartureIdRepositoryImpl])
trait DepartureIdRepository {
  def nextId(): Future[DepartureId]
  def setLatestId(nextId: Int): Future[Unit]
}

@Singleton
class DepartureIdRepositoryImpl @Inject()(mongoComponent: MongoComponent, config: Configuration)(implicit ec: ExecutionContext)
    extends PlayMongoRepository[DepartureIdWrapper](
      mongoComponent = mongoComponent,
      collectionName = "departure-ids-new",
      domainFormat = DepartureIdWrapper.format,
      indexes = Nil,
      extraCodecs = Seq(Codecs.playFormatCodec(DepartureId.formatsDepartureId))
    )
    with DepartureIdRepository {

  private val lastIndexKey         = "last-index"
  private val primaryValue         = "record_id"
  private val featureFlag: Boolean = config.get[Boolean]("feature-flags.testOnly.enabled")

  def nextId(): Future[DepartureId] = {
    val update   = Updates.inc(lastIndexKey, 1)
    val selector = Filters.eq("_id", primaryValue)
    collection
      .findOneAndUpdate(
        filter = selector,
        update = update,
        options = FindOneAndUpdateOptions().upsert(true).bypassDocumentValidation(false)
      )
      .toFuture()
      .map {
        lastIndex =>
          println("last index" + lastIndex.recordId); DepartureId(lastIndex.recordId)
      }

  }

  def setLatestId(nextId: Int): Future[Unit] =
    if (featureFlag) {
      val update = Updates.set(lastIndexKey, nextId)

      val selector = Filters.eq("_id", primaryValue)
      collection
        .updateOne(filter = selector, update = update, options = UpdateOptions().upsert(true).bypassDocumentValidation(false))
        .toFuture()
        .map {
          result =>
            if (result.wasAcknowledged()) {
              if (result.getModifiedCount == 0) Future.failed(new Exception())
              else Future.unit
            } else {
              Future.failed(new Exception("Unable to update next DepartureId"))
            }
        }

    } else
      Future.failed(new Exception("Feature disabled, cannot set next DepartureId"))
}
