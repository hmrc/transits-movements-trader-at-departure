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

import models.DepartureId
import play.api.mvc.ActionBuilder
import javax.inject.Inject
import play.api.mvc.DefaultActionBuilder
import models.MessageType
import models.request.DepartureMessagesRequest
import play.api.mvc.AnyContent

trait AuthenticateGetDepartureMessagesForReadActionProvider {
  def apply(departureId: DepartureId, messageTypes: List[MessageType]): ActionBuilder[DepartureMessagesRequest, AnyContent]
}

class AuthenticateGetDepartureMessagesForReadActionProviderImpl @Inject()(
  authenticate: AuthenticateActionProvider,
  getMessages: AuthenticatedGetDepartureMessagesActionProvider,
  buildDefault: DefaultActionBuilder
) extends AuthenticateGetDepartureMessagesForReadActionProvider {

  def apply(departureId: DepartureId, messageTypes: List[MessageType]): ActionBuilder[DepartureMessagesRequest, AnyContent] =
    buildDefault andThen authenticate() andThen getMessages(departureId, messageTypes)
}
