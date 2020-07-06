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
import models.request.AuthenticatedRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.ActionRefiner
import play.api.mvc.AnyContent
import play.api.mvc.DefaultActionBuilder
import play.api.mvc.Request
import play.api.mvc.Result

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class FakeAuthenticateActionProvider @Inject()(defaultActionBuilder: DefaultActionBuilder, auth: FakeAuthenticateAction)(
  implicit executionContext: ExecutionContext)
    extends AuthenticateActionProvider {

  override def apply(): ActionBuilder[AuthenticatedRequest, AnyContent] =
    defaultActionBuilder andThen auth
}

class FakeAuthenticateAction extends ActionRefiner[Request, AuthenticatedRequest] {
  override protected def refine[A](request: Request[A]): Future[Either[Result, AuthenticatedRequest[A]]] =
    Future.successful(Right(AuthenticatedRequest(request, "eori")))

  override protected def executionContext: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global
}
