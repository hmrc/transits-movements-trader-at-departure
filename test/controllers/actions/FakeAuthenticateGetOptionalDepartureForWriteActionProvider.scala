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

import models.Departure
import models.request.AuthenticatedOptionalDepartureRequest
import models.request.AuthenticatedRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.BodyParser
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.test.Helpers.stubBodyParser

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(departure: Option[Departure]) extends AuthenticateGetOptionalDepartureForWriteActionProvider {
  override def apply(): ActionBuilder[AuthenticatedOptionalDepartureRequest, AnyContent] =
    new ActionBuilder[AuthenticatedOptionalDepartureRequest, AnyContent] {
      override def parser: BodyParser[AnyContent] = stubBodyParser()

      override def invokeBlock[A](request: Request[A], block: AuthenticatedOptionalDepartureRequest[A] => Future[Result]): Future[Result] = {
        val eoriNumber                                    = departure.fold("eori")(_.eoriNumber)
        val authReq                                       = AuthenticatedRequest(request, eoriNumber)
        val req: AuthenticatedOptionalDepartureRequest[A] = AuthenticatedOptionalDepartureRequest(authReq, departure, eoriNumber)
        block(req)
      }

      override protected def executionContext: ExecutionContext = implicitly[ExecutionContext]
    }
}

object FakeAuthenticatedGetOptionalDepartureForWriteActionProvider {

  def apply(departure: Departure): FakeAuthenticatedGetOptionalDepartureForWriteActionProvider =
    new FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(Some(departure))

  def apply(): FakeAuthenticatedGetOptionalDepartureForWriteActionProvider =
    new FakeAuthenticatedGetOptionalDepartureForWriteActionProvider(None)
}
