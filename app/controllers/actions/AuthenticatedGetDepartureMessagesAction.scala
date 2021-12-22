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

package controllers.actions

import javax.inject.Inject
import models.DepartureId
import models.request.AuthenticatedRequest
import models.request.DepartureMessagesRequest
import play.api.Logging
import play.api.mvc.ActionRefiner
import play.api.mvc.Result
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import repositories.DepartureRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import models.MessageType

private[actions] class AuthenticatedGetDepartureMessagesActionProvider @Inject()(
  repository: DepartureRepository
)(implicit ec: ExecutionContext) {

  def apply(departureId: DepartureId, messageTypes: List[MessageType]): ActionRefiner[AuthenticatedRequest, DepartureMessagesRequest] =
    new AuthenticatedGetDepartureMessagesAction(departureId, repository, messageTypes)
}

private[actions] class AuthenticatedGetDepartureMessagesAction(
  departureId: DepartureId,
  repository: DepartureRepository,
  messageTypes: List[MessageType]
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, DepartureMessagesRequest]
    with Logging {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, DepartureMessagesRequest[A]]] =
    repository
      .getMessagesOfType(departureId, request.channel, messageTypes)
      .map {
        case Some(departureMessages) if request.hasMatchingEnrolmentId(departureMessages) && !departureMessages.messages.isEmpty =>
          Right(DepartureMessagesRequest(request, departureId, request.channel, departureMessages.messages))
        case Some(departureMessages) if request.hasMatchingEnrolmentId(departureMessages) =>
          logger.warn(s"No messages of types $messageTypes were found for the given movement")
          Left(NotFound)
        case Some(_) =>
          logger.warn("Attempt to retrieve an departure for another EORI")
          Left(NotFound)
        case None =>
          Left(NotFound)
      }
      .recover {
        case e =>
          logger.error(s"Failed with the following error: $e")
          Left(InternalServerError)
      }
}
