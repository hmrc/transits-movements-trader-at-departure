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
import models.request.DepartureWithoutMessagesRequest
import play.api.mvc.ActionBuilder
import play.api.mvc.AnyContent
import play.api.mvc.BodyParsers

import scala.concurrent.ExecutionContext

class AuthenticatedGetDepartureWithoutMessagesForWriteActionProvider @Inject()(
  lock: LockActionProvider,
  authenticate: AuthenticateActionProvider,
  getDeparture: AuthenticatedGetDepartureWithoutMessagesActionProvider,
  ec: ExecutionContext,
  parser: BodyParsers.Default
) {

  def apply(departureId: DepartureId): ActionBuilder[DepartureWithoutMessagesRequest, AnyContent] =
    lock(departureId) andThen authenticate() andThen getDeparture(departureId)
}
