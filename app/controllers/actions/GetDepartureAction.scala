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
import models.DepartureId
import models.request.AuthenticatedRequest
import models.request.DepartureRequest
import play.api.Logger
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results.BadRequest
import play.api.mvc.Results.InternalServerError
import play.api.mvc.Results.NotFound
import repositories.DepartureRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class GetDepartureActionProvider @Inject()(
  repository: DepartureRepository
)(implicit ec: ExecutionContext) {

  def apply(departureId: DepartureId): ActionRefiner[Request, DepartureRequest] =
    new GetDepartureAction(departureId, repository)
}

private[actions] class GetDepartureAction(
  departureId: DepartureId,
  repository: DepartureRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[Request, DepartureRequest] {

  override protected def refine[A](request: Request[A]): Future[Either[Result, DepartureRequest[A]]] =
    ChannelUtil.getChannel(request) match {
      case None =>
        Future.successful(Left(BadRequest("Missing channel header or incorrect value specified in channel header")))
      case Some(channel) =>
        repository.get(departureId, channel).map {
          case Some(departure) =>
            Right(DepartureRequest(request, departure, channel))
          case None =>
            Left(NotFound)
        }
    }
}

private[actions] class AuthenticatedGetDepartureActionProvider @Inject()(
  repository: DepartureRepository
)(implicit ec: ExecutionContext) {

  def apply(departureId: DepartureId): ActionRefiner[AuthenticatedRequest, DepartureRequest] =
    new AuthenticatedGetDepartureAction(departureId, repository)
}

private[actions] class AuthenticatedGetDepartureAction(
  departureId: DepartureId,
  repository: DepartureRepository
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, DepartureRequest] {

  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, DepartureRequest[A]]] =
    repository
      .get(departureId, request.channel)
      .map {
        case Some(departure) if departure.eoriNumber == request.eoriNumber =>
          Right(DepartureRequest(request.request, departure, request.channel))
        case Some(_) =>
          Logger.warn("Attempt to retrieve an departure for another EORI")
          Left(NotFound)
        case None =>
          Left(NotFound)
      }
      .recover {
        case e =>
          Logger.error(s"Failed with the following error: $e")
          Left(InternalServerError)
      }
}
