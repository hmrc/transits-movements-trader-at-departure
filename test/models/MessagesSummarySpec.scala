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
import generators.ModelGenerators
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json

class MessagesSummarySpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  private val departure = Arbitrary.arbitrary[Departure].sample.value

  "MessagesSummary" - {

    "return declaration link" in {

      val messageId = 1

      Json.toJson(
        MessagesSummary(departure, MessageId.fromIndex(0), None, None)
      ) mustBe Json.obj(
        "departureId" -> departure.departureId,
        "messages" ->
          Json.obj(
            "IE015" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$messageId"
          )
      )

    }

    "return declaration rejected link" in {

      val messageId   = 1
      val rejectionId = 2

      Json.toJson(
        MessagesSummary(departure, MessageId.fromIndex(0), Some(MessageId.fromIndex(1)), None)
      ) mustBe Json.obj(
        "departureId" -> departure.departureId,
        "messages" ->
          Json.obj(
            "IE015" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$messageId",
            "IE016" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$rejectionId"
          )
      )

    }

    "return mrn allocated link" in {

      val messageId    = 1
      val rejectionId  = 2
      val mrnAllocated = 3

      Json.toJson(
        MessagesSummary(departure, MessageId.fromIndex(0), Some(MessageId.fromIndex(1)), Some(MessageId.fromIndex(2)))
      ) mustBe Json.obj(
        "departureId" -> departure.departureId,
        "messages" ->
          Json.obj(
            "IE015" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$messageId",
            "IE016" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$rejectionId",
            "IE028" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$mrnAllocated"
          )
      )

    }

    "return Guarantee not valid link" in {

      val messageId           = 1
      val rejectionId         = 2
      val mrnAllocated        = 3
      val guaranteeNotValidId = 4

      Json.toJson(
        MessagesSummary(departure, MessageId.fromIndex(0), Some(MessageId.fromIndex(1)), Some(MessageId.fromIndex(2)), Some(MessageId.fromIndex(3)))
      ) mustBe Json.obj(
        "departureId" -> departure.departureId,
        "messages" ->
          Json.obj(
            "IE015" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$messageId",
            "IE016" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$rejectionId",
            "IE028" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$mrnAllocated",
            "IE055" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$guaranteeNotValidId"
          )
      )

    }

    "return cancellation decision link" in {

      val messageId           = 1
      val rejectionId         = 2
      val mrnAllocated        = 3
      val guaranteeNotValidId = 4
      val cancellationId      = 5

      Json.toJson(
        MessagesSummary(departure,
                        MessageId.fromIndex(0),
                        Some(MessageId.fromIndex(1)),
                        Some(MessageId.fromIndex(2)),
                        Some(MessageId.fromIndex(3)),
                        Some(MessageId.fromIndex(4)))
      ) mustBe Json.obj(
        "departureId" -> departure.departureId,
        "messages" ->
          Json.obj(
            "IE015" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$messageId",
            "IE016" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$rejectionId",
            "IE028" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$mrnAllocated",
            "IE055" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$guaranteeNotValidId",
            "IE009" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$cancellationId"
          )
      )

    }

    "return cancellation request link" in {

      val messageId             = 1
      val rejectionId           = 2
      val mrnAllocated          = 3
      val guaranteeNotValidId   = 4
      val cancellationId        = 5
      val cancellationRequestId = 6

      Json.toJson(
        MessagesSummary(
          departure,
          MessageId.fromIndex(0),
          Some(MessageId.fromIndex(1)),
          Some(MessageId.fromIndex(2)),
          Some(MessageId.fromIndex(3)),
          Some(MessageId.fromIndex(4)),
          Some(MessageId.fromIndex(5))
        )
      ) mustBe Json.obj(
        "departureId" -> departure.departureId,
        "messages" ->
          Json.obj(
            "IE015" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$messageId",
            "IE016" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$rejectionId",
            "IE028" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$mrnAllocated",
            "IE055" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$guaranteeNotValidId",
            "IE009" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$cancellationId",
            "IE014" -> s"/transits-movements-trader-at-departure/movements/departures/${departure.departureId.index}/messages/$cancellationRequestId"
          )
      )

    }
  }
}
