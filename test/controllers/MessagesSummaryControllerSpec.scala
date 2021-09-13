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

package controllers
import base.SpecBase
import controllers.actions.AuthenticatedGetDepartureWithMessagesForReadActionProvider
import controllers.actions.FakeAuthenticatedGetDepartureWithMessagesForReadActionProvider
import generators.ModelGenerators
import models.Departure
import models.MessageId
import models.MessagesSummary
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.IntegrationPatience
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.GET
import play.api.test.Helpers.contentAsJson
import play.api.test.Helpers.route
import play.api.test.Helpers.running
import play.api.test.Helpers.status
import play.api.test.Helpers._
import services.MessageSummaryService

class MessagesSummaryControllerSpec extends SpecBase with ScalaCheckPropertyChecks with ModelGenerators with BeforeAndAfterEach with IntegrationPatience {

  val departure: Departure = arbitrary[Departure].sample.value
  val departureId          = departure.departureId

  "MessagesSummaryControllerSpec" - {

    "must return" - {

      "return an OK with the message summary when a departure exists" in {
        val fake        = FakeAuthenticatedGetDepartureWithMessagesForReadActionProvider(departure)
        val mockService = mock[MessageSummaryService]

        val declarationMessageId         = MessageId.fromMessageIdValue(1).value
        val declarationRejectedMessageId = MessageId.fromMessageIdValue(2).value
        val messagesSummary              = MessagesSummary(departure, declarationMessageId, Some(declarationRejectedMessageId))

        when(mockService.messagesSummary(any())).thenReturn(messagesSummary)

        val app =
          baseApplicationBuilder
            .overrides(
              bind[AuthenticatedGetDepartureWithMessagesForReadActionProvider].toInstance(fake),
              bind[MessageSummaryService].toInstance(mockService)
            )
            .build()

        running(app) {
          val request = FakeRequest(GET, routes.MessagesSummaryController.messagesSummary(departureId).url)

          val result = route(app, request).value

          status(result) mustEqual OK
          contentAsJson(result) mustEqual Json.toJsObject(messagesSummary)
        }
      }
    }
  }
}
