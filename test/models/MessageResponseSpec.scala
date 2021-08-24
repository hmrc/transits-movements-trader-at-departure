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
import models.MessageType.CancellationDecision
import models.MessageType.ControlDecisionNotification
import models.MessageType.DeclarationRejected
import models.MessageType.GuaranteeNotValid
import models.MessageType.MrnAllocated
import models.MessageType.NoReleaseForTransit
import models.MessageType.PositiveAcknowledgement
import models.MessageType.ReleaseForTransit
import models.MessageType.WriteOffNotification
import play.api.mvc.Request
import play.api.test.FakeRequest

class MessageResponseSpec extends SpecBase {

  "fromRequest" - {
    val responseMessages = Map(
      PositiveAcknowledgement.code     -> PositiveAcknowledgementResponse,
      MrnAllocated.code                -> MrnAllocatedResponse,
      DeclarationRejected.code         -> DepartureRejectedResponse,
      ControlDecisionNotification.code -> ControlDecisionNotificationResponse,
      NoReleaseForTransit.code         -> NoReleaseForTransitResponse,
      ReleaseForTransit.code           -> ReleaseForTransitResponse,
      CancellationDecision.code        -> CancellationDecisionResponse,
      WriteOffNotification.code        -> WriteOffNotificationResponse,
      GuaranteeNotValid.code           -> GuaranteeNotValidResponse
    )

    responseMessages foreach {
      case (str, response) =>
        s"convert from $str to $response" in {
          val request: Request[_] = FakeRequest().withHeaders("X-Message-Type" -> str)
          MessageResponse.fromRequest(request) mustBe Right(response)
        }
    }

    "return an invalid message type for an invalid message type" in {
      val request: Request[_] = FakeRequest().withHeaders("X-Message-Type" -> "Fake")
      MessageResponse.fromRequest(request) mustBe Left(InvalidMessageType("Received the following invalid response for X-Message-Type: Fake"))
    }

    "return an invalid message type for a missing message type header" in {
      val request: Request[_] = FakeRequest()
      MessageResponse.fromRequest(request) mustBe Left(InvalidMessageType("A 'X-Message-Type' header must be defined in the request."))
    }

  }
}
