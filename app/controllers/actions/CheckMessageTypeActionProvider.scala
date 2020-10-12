/*
 * Copyright 2020 HM Revenue & Customs
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

import javax.inject.Inject
import models.CancellationDecisionResponse
import models.ControlDecisionNotificationResponse
import models.DepartureId
import models.DepartureRejectedResponse
import models.GuaranteeNotValidResponse
import models.MessageResponse
import models.MessageType
import models.MrnAllocatedResponse
import models.NoReleaseForTransitResponse
import models.PositiveAcknowledgementResponse
import models.ReleaseForTransitResponse
import models.WriteOffNotificationResponse
import models.request.AuthenticatedRequest
import models.request.DepartureRequest
import models.request.DepartureResponseRequest
import play.api.Logger
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import repositories.DepartureRepository
import play.api.mvc.Results.BadRequest

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class CheckMessageTypeActionProvider @Inject()()(implicit ec: ExecutionContext) {

  def apply(): ActionRefiner[DepartureRequest, DepartureResponseRequest] =
    new CheckMessageTypeAction()
}

// case match with if

class CheckMessageTypeAction()(implicit val executionContext: ExecutionContext) extends ActionRefiner[DepartureRequest, DepartureResponseRequest] {
  override protected def refine[A](request: DepartureRequest[A]): Future[Either[Result, DepartureResponseRequest[A]]] = {

    def successMessage(response: MessageResponse): Future[Right[Result, DepartureResponseRequest[A]]] =
      Future.successful(Right(DepartureResponseRequest(request, response)))

    def badRequestError(message: String): Future[Left[Result, DepartureResponseRequest[A]]] = {
      Logger.warn(message)
      Future.successful(Left(BadRequest(message)))
    }

    request.headers.get("X-Message-Type") match {
      case Some(MessageType.PositiveAcknowledgement.code)     => successMessage(PositiveAcknowledgementResponse)
      case Some(MessageType.MrnAllocated.code)                => successMessage(MrnAllocatedResponse)
      case Some(MessageType.DeclarationRejected.code)         => successMessage(DepartureRejectedResponse)
      case Some(MessageType.ControlDecisionNotification.code) => successMessage(ControlDecisionNotificationResponse)
      case Some(MessageType.NoReleaseForTransit.code)         => successMessage(NoReleaseForTransitResponse)
      case Some(MessageType.ReleaseForTransit.code)           => successMessage(ReleaseForTransitResponse)
      case Some(MessageType.CancellationDecision.code)        => successMessage(CancellationDecisionResponse)
      case Some(MessageType.WriteOffNotification.code)        => successMessage(WriteOffNotificationResponse)
      case Some(MessageType.GuaranteeNotValid.code)           => successMessage(GuaranteeNotValidResponse)
      case Some(invalidType)                                  => badRequestError(s"Received the following invalid response for X-Message-Type: $invalidType")
      case None                                               => badRequestError(s"Missing X-Message-Type header")
    }
  }
}
