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

import models.request.AuthenticatedClientRequest
import models.request.AuthenticatedRequest
import play.api.mvc.ActionTransformer
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.AuthorisedFunctions
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import javax.inject.Inject

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class AuthenticatedClientIdAction @Inject()(override val authConnector: AuthConnector)(implicit val executionContext: ExecutionContext)
    extends ActionTransformer[AuthenticatedRequest, AuthenticatedClientRequest]
    with AuthorisedFunctions {

  override protected def transform[A](request: AuthenticatedRequest[A]): Future[AuthenticatedClientRequest[A]] = {
    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers)
    authorised()
      .retrieve(Retrievals.clientId)(clientOpt => Future.successful(AuthenticatedClientRequest(request, request.channel, request.eoriNumber, clientOpt)))
  }

}
