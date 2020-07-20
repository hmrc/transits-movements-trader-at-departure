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

package controllers

import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import javax.inject.Inject
import models.response.ResponseDepartureWithMessages
import models.response.ResponseMessage
import models.DepartureId
import models.MessageId
import models.MessageStatus.SubmissionFailed
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext

class MessagesController @Inject()(
  cc: ControllerComponents,
  authenticateForRead: AuthenticatedGetDepartureForReadActionProvider
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def getMessages(departureId: DepartureId): Action[AnyContent] = authenticateForRead(departureId) {
    implicit request =>
      Ok(Json.toJsObject(ResponseDepartureWithMessages.build(request.departure)))
  }

  def getMessage(departureId: DepartureId, messageId: MessageId): Action[AnyContent] = authenticateForRead(departureId) {
    implicit request =>
      val messages = request.departure.messages.toList

      if (messages.isDefinedAt(messageId.index) && messages(messageId.index).optStatus != Some(SubmissionFailed))
        Ok(Json.toJsObject(ResponseMessage.build(departureId, messageId, messages(messageId.index))))
      else NotFound
  }
}
