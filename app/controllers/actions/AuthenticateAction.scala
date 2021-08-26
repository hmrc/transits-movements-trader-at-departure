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

import config.AppConfig
import javax.inject.Inject
import models.request.AuthenticatedRequest
import play.api.Logging
import play.api.mvc.ActionRefiner
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results.BadRequest
import play.api.mvc.Results.Forbidden
import play.api.mvc.Results.Unauthorized
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[actions] class AuthenticateAction @Inject()(override val authConnector: AuthConnector, config: AppConfig)(
  implicit val executionContext: ExecutionContext)
    extends ActionRefiner[Request, AuthenticatedRequest]
    with AuthorisedFunctions
    with Logging {

  private val enrolmentIdentifierKey: String = "VATRegNoTURN"

  override protected def refine[A](request: Request[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    ChannelUtil.getChannel(request) match {
      case None =>
        Future.successful(Left(BadRequest("Missing channel header or incorrect value specified in channel header")))
      case Some(channel) =>
        implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequest(request)
        authorised(Enrolment(config.enrolmentKey)).retrieve(Retrievals.authorisedEnrolments) {
          enrolments =>
            val eoriNumber = (for {
              enrolment  <- enrolments.enrolments.find(_.key.equals(config.enrolmentKey))
              identifier <- enrolment.getIdentifier(enrolmentIdentifierKey)
            } yield identifier.value).getOrElse(throw InsufficientEnrolments(s"Unable to retrieve enrolment for $enrolmentIdentifierKey"))
            Future.successful(Right(AuthenticatedRequest(request, channel, eoriNumber)))
        }
    }
  }.recover {
    case e: InsufficientEnrolments =>
      logger.warn(s"Failed to authorise with the following exception: $e")
      Left(Forbidden)
    case e: AuthorisationException =>
      logger.warn(s"Failed to authorise with the following exception: $e")
      Left(Unauthorized)
  }
}
