/*
 * Copyright 2022 HM Revenue & Customs
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
import controllers.routes
import generators.ModelGenerators
import models.ChannelType.Api
import models.request.DepartureResponseRequest
import models.request.DepartureWithoutMessagesRequest
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.HeaderNames
import play.api.http.HttpVerbs
import play.api.mvc.Request
import play.api.test.FakeRequest

import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import scala.xml.NodeSeq
import models.request.AuthenticatedRequest
import cats.data.Ior
import org.scalacheck.Arbitrary.arbitrary

class DepartureMessageNotificationSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators with HttpVerbs {

  val responseGenerator = Gen.oneOf(MessageResponse.values)

  "fromRequest" - {
    val testBody    = <text></text>
    val bodyLength  = testBody.toString.getBytes(StandardCharsets.UTF_8).length
    val enrolmentId = EnrolmentId(Ior.right(EORINumber("eori")))
    "produces the expected model" in {
      val response = responseGenerator.sample.value

      val departureWithoutMessages = arbitrary[DepartureWithoutMessages].sample.value

      val messageSender = MessageSender(departureWithoutMessages.departureId, departureWithoutMessages.nextMessageCorrelationId)

      val request =
        AuthenticatedRequest(
          FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withBody[NodeSeq](testBody)
            .withHeaders(HeaderNames.CONTENT_LENGTH -> bodyLength.toString),
          departureWithoutMessages.channel,
          enrolmentId
        )

      val departureWithoutMessagesRequest = DepartureWithoutMessagesRequest(request, departureWithoutMessages, Api)
      val responseRequest                 = DepartureResponseRequest(departureWithoutMessagesRequest, response)

      val now = LocalDateTime.now()

      val departureIdIndex = departureWithoutMessages.departureId.index

      val expectedNotification =
        DepartureMessageNotification(
          s"/customs/transits/movements/departures/$departureIdIndex/messages/${departureWithoutMessages.nextMessageCorrelationId}",
          s"/customs/transits/movements/departures/$departureIdIndex",
          departureWithoutMessages.eoriNumber,
          departureWithoutMessages.departureId,
          departureWithoutMessages.nextMessageId,
          now,
          response.messageType,
          Some(testBody)
        )

      val testNotification = DepartureMessageNotification.fromRequest(responseRequest, now)

      testNotification mustEqual expectedNotification
    }

    "does not include the message body when it is over 100kb" in {
      val response                 = responseGenerator.sample.value
      val departureWithoutMessages = arbitrary[DepartureWithoutMessages].sample.value

      val messageSender = MessageSender(departureWithoutMessages.departureId, departureWithoutMessages.nextMessageCorrelationId)

      val request =
        AuthenticatedRequest(
          FakeRequest(POST, routes.NCTSMessageController.post(messageSender).url)
            .withBody[NodeSeq](testBody)
            .withHeaders(HeaderNames.CONTENT_LENGTH -> "100001"),
          departureWithoutMessages.channel,
          enrolmentId
        )

      val departureRequest = DepartureWithoutMessagesRequest(request, departureWithoutMessages, Api)
      val responseRequest  = DepartureResponseRequest(departureRequest, response)

      val now = LocalDateTime.now()

      val departureIdIndex = departureWithoutMessages.departureId.index

      val expectedNotification =
        DepartureMessageNotification(
          s"/customs/transits/movements/departures/$departureIdIndex/messages/${departureWithoutMessages.nextMessageCorrelationId}",
          s"/customs/transits/movements/departures/$departureIdIndex",
          departureWithoutMessages.eoriNumber,
          departureWithoutMessages.departureId,
          departureWithoutMessages.nextMessageId,
          now,
          response.messageType,
          None
        )

      val testNotification = DepartureMessageNotification.fromRequest(responseRequest, now)

      testNotification mustEqual expectedNotification
    }
  }

  "fromDepartureAndResponse" - {
    val testBody   = <text></text>
    val bodyLength = testBody.toString.getBytes(StandardCharsets.UTF_8).length

    "produces the expected model" in {
      val response  = responseGenerator.sample.value
      val departure = arbitraryDeparture.arbitrary.sample.value

      val request: FakeRequest[NodeSeq] = FakeRequest()
        .withBody[NodeSeq](testBody)
        .withHeaders(HeaderNames.CONTENT_LENGTH -> bodyLength.toString)

      val now = LocalDateTime.now()

      val expectedNotification =
        DepartureMessageNotification(
          s"/customs/transits/movements/departures/${departure.departureId.index}/messages/${departure.messages.length + 1}",
          s"/customs/transits/movements/departures/${departure.departureId.index}",
          departure.eoriNumber,
          departure.departureId,
          MessageId(departure.messages.length + 1),
          now,
          response.messageType,
          Some(testBody)
        )

      val testNotification = DepartureMessageNotification.fromDepartureAndResponse(departure, response, now, request)

      testNotification mustEqual expectedNotification
    }

    "does not include the message body when it is over 100kb" in {
      val response  = responseGenerator.sample.value
      val departure = arbitraryDeparture.arbitrary.sample.value

      val request: Request[NodeSeq] = FakeRequest()
        .withBody[NodeSeq](testBody)
        .withHeaders(HeaderNames.CONTENT_LENGTH -> "100001")

      val now = LocalDateTime.now()

      val expectedNotification =
        DepartureMessageNotification(
          s"/customs/transits/movements/departures/${departure.departureId.index}/messages/${departure.messages.length + 1}",
          s"/customs/transits/movements/departures/${departure.departureId.index}",
          departure.eoriNumber,
          departure.departureId,
          MessageId(departure.messages.length + 1),
          now,
          response.messageType,
          None
        )

      val testNotification = DepartureMessageNotification.fromDepartureAndResponse(departure, response, now, request)

      testNotification mustEqual expectedNotification
    }
  }
}
