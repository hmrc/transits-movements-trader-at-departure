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

import java.time.LocalDateTime

import base.SpecBase
import controllers.routes
import generators.ModelGenerators
import models.ChannelType.api
import models.request.DepartureRequest
import models.request.DepartureResponseRequest
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.HttpVerbs
import play.api.test.FakeRequest

import scala.xml.NodeSeq

class DepartureMessageNotificationSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with HttpVerbs {

  val responseGenerator = Gen.oneOf(MessageResponse.values)

  "fromRequest" - {
    "produces the expected model" in {
      val response      = responseGenerator.sample.value
      val departure     = arbitraryDeparture.arbitrary.sample.value
      val messageSender = MessageSender(departure.departureId, departure.messages.last.messageCorrelationId)

      val request = FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
        .withBody[NodeSeq](<text></text>)
      val departureRequest = DepartureRequest(request, departure, api)
      val responseRequest  = DepartureResponseRequest(departureRequest, response)

      val now = LocalDateTime.now()

      val expectedNotification =
        DepartureMessageNotification(
          s"/customs/transits/movements/departures/${departure.departureId.index}/messages/${departure.messages.length + 1}",
          s"/customs/transits/movements/departures/${departure.departureId.index}",
          departure.departureId,
          MessageId.fromIndex(departure.messages.length),
          now,
          response.messageType
        )

      val testNotification = DepartureMessageNotification.fromRequest(responseRequest, now)

      testNotification mustEqual expectedNotification
    }
  }
}
