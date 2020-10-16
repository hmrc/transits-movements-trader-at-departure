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

import audit.AuditService
import audit.AuditType.DepartureCancellationRequestSubmitted
import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import controllers.actions.AuthenticatedGetDepartureForWriteActionProvider
import javax.inject.Inject
import models.MessageStatus.SubmissionFailed
import models._
import models.request.DepartureRequest
import models.response.ResponseDepartureWithMessages
import models.response.ResponseMessage
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.DepartureService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessagesController @Inject()(
  cc: ControllerComponents,
  authenticateForRead: AuthenticatedGetDepartureForReadActionProvider,
  authenticateForWrite: AuthenticatedGetDepartureForWriteActionProvider,
  departureService: DepartureService,
  auditService: AuditService,
  submitMessageService: SubmitMessageService
)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post(departureId: DepartureId): Action[NodeSeq] = authenticateForWrite(departureId).async(parse.xml) {
    implicit request: DepartureRequest[NodeSeq] =>
      MessageType.getMessageType(request.body) match {
        case Some(MessageType.DeclarationCancellationRequest) =>
          departureService
            .makeMessageWithStatus(request.departure.departureId, request.departure.nextMessageCorrelationId, MessageType.DeclarationCancellationRequest)(
              request.body) match {
            case Right(message) => {
              request.departure.status.transition(MessageReceivedEvent.DeclarationCancellationRequest) match {
                case Right(status) =>
                  submitMessageService
                    .submitMessage(departureId, request.departure.nextMessageId.index, message, status)
                    .map {
                      case SubmissionProcessingResult.SubmissionSuccess =>
                        auditService.auditEvent(DepartureCancellationRequestSubmitted, request.body)
                        Accepted
                          .withHeaders("Location" -> routes.MessagesController.getMessage(request.departure.departureId, request.departure.nextMessageId).url)

                      case SubmissionProcessingResult.SubmissionFailureInternal =>
                        InternalServerError

                      case SubmissionProcessingResult.SubmissionFailureExternal =>
                        BadGateway
                    }
                case Left(error) =>
                  Future.successful(BadRequest(error.reason))
              }
            }
            case Left(error) =>
              Logger.warn(error.message)
              Future.successful(BadRequest(error.message))
          }
        case _ =>
          Future.successful(NotImplemented)
      }
  }

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
