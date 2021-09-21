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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import org.scalacheck.Arbitrary.arbitrary
import play.api.libs.json.Json

import java.time.LocalDateTime

class DepartureWithoutMessagesSpec extends SpecBase with ModelGenerators {

  "DepartureWithoutMessages" - {
    "must Serialise and return latest message type" in {

      forAll(arbitrary[Departure], arbitrary[MessageWithStatus]) {
        (x, y) =>
          val expectedDateTime             = LocalDateTime.now
          val expectedDateTimeMinusMinutes = LocalDateTime.now.minusMinutes(10)
          val expectedDateTimeMinusHours   = LocalDateTime.now.minusHours(2)
          val expectedDateTimeMinusDays    = LocalDateTime.now.minusDays(1)

          val message1        = y.copy(dateTime = expectedDateTimeMinusDays)
          val message2        = y.copy(dateTime = expectedDateTimeMinusMinutes)
          val message3        = y.copy(dateTime = expectedDateTimeMinusHours)
          val expectedMessage = y.copy(dateTime = expectedDateTime)

          val departure = x.copy(messages = NonEmptyList(message1, List(message2, message3, expectedMessage)))

          val departureWithoutMessages: DepartureWithoutMessages = DepartureWithoutMessages.fromDeparture(departure)

          Json.toJsObject(departureWithoutMessages).validate[Departure] mustBe expectedMessage.messageId
      }
    }
  }

}
