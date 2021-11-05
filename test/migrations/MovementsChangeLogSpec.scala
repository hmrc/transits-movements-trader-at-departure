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

import base.SpecBase
import cats.syntax.all._
import models.ChannelType
import models.DepartureId
import models.DepartureStatus
import models.MessageStatus
import models.MessageType
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.play.json.collection.Helpers.idWrites
import reactivemongo.play.json.collection.JSONCollection
import repositories.DepartureRepository
import java.time.LocalDateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.xml.NodeSeq

class MovementsChangeLogSpec extends SpecBase with IntegrationPatience with BeforeAndAfterAll with GuiceOneAppPerSuite {

  val departureIds = (1 to 20).map(DepartureId.apply)
  val eori         = "123456789000"

  override protected def afterAll(): Unit = {
    val mongo = app.injector.instanceOf[ReactiveMongoApi]

    val dropDatabase = for {
      db <- mongo.database
      _  <- db.drop()
    } yield ()

    dropDatabase.futureValue
  }

  override def beforeAll(): Unit = {
    import models.MongoDateTimeFormats._
    import utils.NodeSeqFormat._

    val mongo = app.injector.instanceOf[ReactiveMongoApi]

    val insertDepartures = for {
      db <- mongo.database

      _ <- db.drop()

      coll = db.collection[JSONCollection](DepartureRepository.collectionName)

      _ <- coll.insert.many {
        for (id <- departureIds)
          yield
            Json.obj(
              "_id"                      -> id.index,
              "channel"                  -> ChannelType.Web.toString,
              "eoriNumber"               -> eori,
              "referenceNumber"          -> Random.alphanumeric.take(20).mkString,
              "status"                   -> DepartureStatus.DepartureSubmitted.toString,
              "created"                  -> LocalDateTime.of(2021, 7, 15, 12, 12),
              "lastUpdated"              -> LocalDateTime.of(2021, 7, 15, 12, 13),
              "nextMessageCorrelationId" -> 2,
              "messages" -> Json.arr(
                Json.obj(
                  "dateTime"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                  "messageType"          -> MessageType.DepartureDeclaration.toString,
                  "message"              -> NodeSeq.fromSeq(Seq(<CC015B></CC015B>)),
                  "status"               -> MessageStatus.SubmissionSucceeded.toString,
                  "messageCorrelationId" -> 1
                ),
                Json.obj(
                  "dateTime"             -> LocalDateTime.of(2021, 7, 15, 12, 12),
                  "messageType"          -> MessageType.PositiveAcknowledgement.toString,
                  "message"              -> NodeSeq.fromSeq(Seq(<CC928A></CC928A>)),
                  "messageCorrelationId" -> 1
                ),
                Json.obj(
                  "dateTime"             -> LocalDateTime.of(2021, 7, 15, 12, 13),
                  "messageType"          -> MessageType.MrnAllocated.toString,
                  "message"              -> NodeSeq.fromSeq(Seq(<CC028A></CC028A>)),
                  "messageCorrelationId" -> 1
                )
              )
            )
      }
    } yield ()

    insertDepartures.futureValue
  }

  "MovementsChangeLog" - {
    "addMessageIdToMessages" - {
      "should add message ID to messages in the departures collection" in {
        val repo   = app.injector.instanceOf[DepartureRepository]
        val runner = app.injector.instanceOf[MigrationRunner]

        runner.runMigrations().futureValue

        departureIds.foreach {
          departureId =>
            val departure = repo.get(departureId).futureValue.value
            departure.messages.mapWithIndex {
              case (message, index) =>
                message.messageId.value mustBe (index + 1)
            }
        }
      }
    }
  }
}
