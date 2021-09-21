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

package models

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageType.DeclarationRejected
import models.MessageType.DepartureDeclaration
import models.MessageType.MrnAllocated
import models.MessageType.PositiveAcknowledgement
import org.scalacheck.Arbitrary.arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import play.api.libs.json.JsObject
import play.api.libs.json.Json

import java.time.LocalDateTime

class DepartureWithoutMessagesSpec extends SpecBase with ModelGenerators with MongoDateTimeFormats {

  "DepartureWithoutMessages" - {
    "apply" - {
      "must return latest message type" in {

        forAll(arbitrary[Departure], arbitrary[MessageWithStatus]) {
          (departure, message) =>
            val expectedDateTime             = LocalDateTime.now
            val expectedDateTimeMinusMinutes = LocalDateTime.now.minusMinutes(10)
            val expectedDateTimeMinusHours   = LocalDateTime.now.minusHours(2)
            val expectedDateTimeMinusDays    = LocalDateTime.now.minusDays(1)

            val message1        = message.copy(dateTime = expectedDateTimeMinusDays, messageType = PositiveAcknowledgement)
            val message2        = message.copy(dateTime = expectedDateTimeMinusMinutes, messageType = DepartureDeclaration)
            val message3        = message.copy(dateTime = expectedDateTimeMinusHours, messageType = MrnAllocated)
            val expectedMessage = message.copy(dateTime = expectedDateTime, messageType = DeclarationRejected)

            val departureWithDateTime = departure.copy(
              messages = NonEmptyList(message1, List(message2, message3, expectedMessage))
            )

            val result = DepartureWithoutMessages.fromDeparture(departureWithDateTime)

            result.latestMessage mustBe DeclarationRejected
        }
      }
    }

    "must Serialise and return latest message type" in {

      forAll(arbitrary[Departure]) {
        departure =>
          val json = Json.obj(
            "_id"                      -> departure.departureId,
            "channel"                  -> departure.channel,
            "eoriNumber"               -> departure.eoriNumber,
            "movementReferenceNumber"  -> departure.movementReferenceNumber,
            "referenceNumber"          -> departure.referenceNumber,
            "status"                   -> departure.status,
            "created"                  -> departure.created,
            "lastUpdated"              -> departure.lastUpdated,
            "notificationBox"          -> departure.notificationBox,
            "nextMessageId"            -> departure.nextMessageId,
            "nextMessageCorrelationId" -> departure.nextMessageCorrelationId,
            "messages" -> Json.arr(
              Json.obj(
                "messageId"            -> 1,
                "dateTime"             -> LocalDateTime.now.minusHours(2),
                "messageType"          -> "IE928",
                "message"              -> "<foo></foo>",
                "messageCorrelationId" -> 1,
                "messageJson"          -> ""
              ),
              Json.obj(
                "messageId"            -> 2,
                "dateTime"             -> LocalDateTime.now,
                "messageType"          -> "IE015",
                "message"              -> "<foo></foo>",
                "messageCorrelationId" -> 1,
                "messageJson"          -> ""
              ),
              Json.obj(
                "messageId"            -> 3,
                "dateTime"             -> LocalDateTime.now.minusDays(5),
                "messageType"          -> "IE016",
                "message"              -> "<foo></foo>",
                "messageCorrelationId" -> 1,
                "messageJson"          -> ""
              )
            )
          )

          val result = json.validate[DepartureWithoutMessages].asOpt.value

          result.latestMessage mustBe DepartureDeclaration
      }
    }
  }
}
