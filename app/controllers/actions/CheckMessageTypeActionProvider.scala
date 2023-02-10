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

package controllers.actions

import models.CancellationDecisionResponse
import models.ControlDecisionNotificationResponse
import models.DepartureRejectedResponse
import models.GuaranteeNotValidResponse
import models.MessageResponse
import models.MessageType
import models.MrnAllocatedResponse
import models.NoReleaseForTransitResponse
import models.PositiveAcknowledgementResponse
import models.ReleaseForTransitResponse
import models.WriteOffNotificationResponse
import models.XMLSubmissionNegativeAcknowledgementResponse
import models.request.DepartureResponseRequest
import models.request.DepartureWithoutMessagesRequest
import play.api.Logging
import play.api.mvc.ActionRefiner
import play.api.mvc.Result
import play.api.mvc.Results.BadRequest
import javax.inject.Inject

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class CheckMessageTypeActionProvider @Inject()()(implicit ec: ExecutionContext) {

  def apply(): ActionRefiner[DepartureWithoutMessagesRequest, DepartureResponseRequest] =
    new CheckMessageTypeAction()
}

class CheckMessageTypeAction()(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[DepartureWithoutMessagesRequest, DepartureResponseRequest]
    with Logging {

  override protected def refine[A](request: DepartureWithoutMessagesRequest[A]): Future[Either[Result, DepartureResponseRequest[A]]] = {

    def successMessage(response: MessageResponse): Future[Right[Result, DepartureResponseRequest[A]]] =
      Future.successful(Right(DepartureResponseRequest(request, response)))

    def badRequestError(message: String): Future[Left[Result, DepartureResponseRequest[A]]] = {
      logger.warn(message)
      Future.successful(Left(BadRequest(message)))
    }

    request.headers.get("X-Message-Type") match {
      case Some(MessageType.PositiveAcknowledgement.code)              => successMessage(PositiveAcknowledgementResponse)
      case Some(MessageType.MrnAllocated.code)                         => successMessage(MrnAllocatedResponse)
      case Some(MessageType.DeclarationRejected.code)                  => successMessage(DepartureRejectedResponse)
      case Some(MessageType.ControlDecisionNotification.code)          => successMessage(ControlDecisionNotificationResponse)
      case Some(MessageType.NoReleaseForTransit.code)                  => successMessage(NoReleaseForTransitResponse)
      case Some(MessageType.ReleaseForTransit.code)                    => successMessage(ReleaseForTransitResponse)
      case Some(MessageType.CancellationDecision.code)                 => successMessage(CancellationDecisionResponse)
      case Some(MessageType.WriteOffNotification.code)                 => successMessage(WriteOffNotificationResponse)
      case Some(MessageType.GuaranteeNotValid.code)                    => successMessage(GuaranteeNotValidResponse)
      case Some(MessageType.XMLSubmissionNegativeAcknowledgement.code) => successMessage(XMLSubmissionNegativeAcknowledgementResponse)
      case Some(invalidType)                                           => badRequestError(s"Received the following invalid response for X-Message-Type: $invalidType")
      case None                                                        => badRequestError("A 'X-Message-Type' header must be defined in the request.")
    }
  }
}
