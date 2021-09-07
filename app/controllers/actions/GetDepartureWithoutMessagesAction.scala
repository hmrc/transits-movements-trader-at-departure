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
import models.request.DepartureWithoutMessagesRequest
import play.api.Logging
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import play.api.mvc.Results.Ok
import repositories.DepartureRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class GetDepartureWithoutMessagesActionProvider @Inject()(
  repository: DepartureRepository
)(implicit ec: ExecutionContext) {

  def apply(departureId: DepartureId): ActionRefiner[Request, DepartureWithoutMessagesRequest] =
    new GetDepartureWithoutMessagesAction(departureId, repository)
}

private[actions] class GetDepartureWithoutMessagesAction(
  departureId: DepartureId,
  repository: DepartureRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[Request, DepartureWithoutMessagesRequest] 
    with Logging {

  override protected def refine[A](request: Request[A]): Future[Either[Result, DepartureWithoutMessagesRequest[A]]] =
    repository.getWithoutMessages(departureId).map {
      case Some(departure) =>
        Right(DepartureWithoutMessagesRequest(request, departure, departure.channel))
      case None =>
        logger.info(s"[GetDepartureAction] Unable to retrieve departure message for departure id: ${departureId.index}")
        Left(Ok)
    }
}

private[actions] class AuthenticatedGetDepartureWithoutMessagesActionProvider @Inject()(
  repository: DepartureRepository
)(implicit ec: ExecutionContext) {

  def apply(departureId: DepartureId): ActionRefiner[AuthenticatedRequest, DepartureWithoutMessagesRequest] =
    new AuthenticatedGetDepartureWithoutMessagesAction(departureId, repository)
}

private[actions] class AuthenticatedGetDepartureWithoutMessagesAction(
  departureId: DepartureId,
  repository: DepartureRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, DepartureWithoutMessagesRequest]
    with Logging {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, DepartureWithoutMessagesRequest[A]]] =
    repository
      .getWithoutMessages(departureId, request.channel)
      .map {
        case Some(departure) if departure.eoriNumber == request.eoriNumber =>
          Right(DepartureWithoutMessagesRequest(request.request, departure, request.channel))
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
