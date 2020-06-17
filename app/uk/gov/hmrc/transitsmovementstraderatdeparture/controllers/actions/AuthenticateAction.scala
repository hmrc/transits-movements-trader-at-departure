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

package uk.gov.hmrc.transitsmovementstraderatdeparture.controllers.actions

import javax.inject.Inject
import play.api.Logger
import play.api.mvc.{ActionRefiner, Request, Result}
import play.api.mvc.Results.Unauthorized
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisationException, AuthorisedFunctions, Enrolment, InsufficientEnrolments}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.transitsmovementstraderatdeparture.config.AppConfig
import uk.gov.hmrc.transitsmovementstraderatdeparture.models.request.AuthenticatedRequest

import scala.concurrent.{ExecutionContext, Future}

private[actions] class AuthenticateAction @Inject()(override val authConnector: AuthConnector, config: AppConfig)(
  implicit val executionContext: ExecutionContext)
  extends ActionRefiner[Request, AuthenticatedRequest]
    with AuthorisedFunctions {

  private val enrolmentIdentifierKey: String = "VATRegNoTURN"

  override protected def refine[A](request: Request[A]): Future[Either[Result, AuthenticatedRequest[A]]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers)
    authorised(Enrolment(config.enrolmentKey)).retrieve(Retrievals.authorisedEnrolments) {
      enrolments =>
        val eoriNumber = (for {
          enrolment  <- enrolments.enrolments.find(_.key.equals(config.enrolmentKey))
          identifier <- enrolment.getIdentifier(enrolmentIdentifierKey)
        } yield identifier.value).getOrElse(throw InsufficientEnrolments(s"Unable to retrieve enrolment for $enrolmentIdentifierKey"))
        Future.successful(Right(AuthenticatedRequest(request, eoriNumber)))
    }
    }.recover {
    case e: AuthorisationException =>
      Logger.warn(s"Failed to authorise with the following exception: $e")
      Left(Unauthorized)
  }
}
