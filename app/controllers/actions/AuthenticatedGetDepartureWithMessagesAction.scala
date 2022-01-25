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

package controllers.actions

import javax.inject.Inject
import models.DepartureId
import models.request.AuthenticatedRequest
import models.request.DepartureWithMessagesRequest
import play.api.Logging
import play.api.mvc.ActionRefiner
import play.api.mvc.Result
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import repositories.DepartureRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import audit.AuditService

private[actions] class AuthenticatedGetDepartureWithMessagesActionProvider @Inject()(
  repository: DepartureRepository,
  auditService: AuditService
)(implicit ec: ExecutionContext) {

  def apply(departureId: DepartureId): ActionRefiner[AuthenticatedRequest, DepartureWithMessagesRequest] =
    new AuthenticatedGetDepartureWithMessagesAction(departureId, repository, auditService)
}

private[actions] class AuthenticatedGetDepartureWithMessagesAction(
  departureId: DepartureId,
  repository: DepartureRepository,
  auditService: AuditService
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, DepartureWithMessagesRequest]
    with Logging {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, DepartureWithMessagesRequest[A]]] =
    repository
      .get(departureId, request.channel)
      .map {
        case Some(departure) if request.hasMatchingEnrolmentId(departure) =>
          Right(DepartureWithMessagesRequest(request, departure, request.channel))
        case Some(_) =>
          logger.warn("Attempt to retrieve an departure for another EORI")
          Left(NotFound)
        case None =>
          auditService.auditCustomerRequestedMissingMovementEvent(request, departureId)
          Left(NotFound)
      }
      .recover {
        case e =>
          logger.error(s"Failed with the following error: $e")
          Left(InternalServerError)
      }
}
