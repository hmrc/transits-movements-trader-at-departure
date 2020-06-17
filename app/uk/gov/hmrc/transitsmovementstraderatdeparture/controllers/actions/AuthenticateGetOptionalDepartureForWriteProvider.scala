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
import play.api.mvc.{ActionBuilder, ActionFunction, AnyContent, Result}
import uk.gov.hmrc.transitsmovementstraderatdeparture.models.request.{AuthenticatedOptionalDepartureRequest, AuthenticatedRequest}
import uk.gov.hmrc.transitsmovementstraderatdeparture.repositories.{DepartureRepository, LockRepository}

import scala.concurrent.{ExecutionContext, Future}

trait AuthenticateGetOptionalDepartureForWriteActionProvider {
  def apply(): ActionBuilder[AuthenticatedOptionalDepartureRequest, AnyContent]
}

class AuthenticateGetOptionalDepartureForWriteActionProviderImpl @Inject()(
                                                                             authenticate: AuthenticateActionProvider,
                                                                             departureRepository: DepartureRepository,
                                                                             lockRepository: LockRepository,
                                                                             ec: ExecutionContext
                                                                         ) extends AuthenticateGetOptionalDepartureForWriteActionProvider {

  def apply(): ActionBuilder[AuthenticatedOptionalDepartureRequest, AnyContent] =
    authenticate() andThen new AuthenticateGetOptionalDepartureForWriteAction(departureRepository, lockRepository, ec)
}

class AuthenticateGetOptionalDepartureForWriteAction(departureRepository: DepartureRepository,
                                                     lockRepository: LockRepository,
                                                     implicit protected val executionContext: ExecutionContext) extends ActionFunction[AuthenticatedRequest, AuthenticatedOptionalDepartureRequest] {
  override def invokeBlock[A](request: AuthenticatedRequest[A], block: AuthenticatedOptionalDepartureRequest[A] => Future[Result]): Future[Result] = ???
}