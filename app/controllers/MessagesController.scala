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

package controllers

import javax.inject.Inject
import audit.AuditService
import audit.AuditType.DepartureCancellationRequestSubmitted
import com.kenshoo.play.metrics.Metrics
import controllers.actions.AuthenticatedGetDepartureForReadActionProvider
import controllers.actions.AuthenticatedGetDepartureForWriteActionProvider
import metrics.HasActionMetrics
import models.MessageStatus.SubmissionFailed
import models._
import models.request.DepartureRequest
import models.response.ResponseDepartureWithMessages
import models.response.ResponseMessage
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import services.DepartureService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq
import java.time.OffsetDateTime

class MessagesController @Inject()(
  cc: ControllerComponents,
  authenticateForRead: AuthenticatedGetDepartureForReadActionProvider,
  authenticateForWrite: AuthenticatedGetDepartureForWriteActionProvider,
  departureService: DepartureService,
  auditService: AuditService,
  submitMessageService: SubmitMessageService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  lazy val messagesCount = histo("get-all-departure-messages-count")

  def post(departureId: DepartureId): Action[NodeSeq] =
    withMetricsTimerAction("post-submit-message") {
      authenticateForWrite(departureId).async(parse.xml) {
        implicit request: DepartureRequest[NodeSeq] =>
          MessageType.getMessageType(request.body) match {
            case Some(MessageType.DeclarationCancellationRequest) =>
              departureService
                .makeMessageWithStatus(request.departure.departureId, request.departure.nextMessageCorrelationId, MessageType.DeclarationCancellationRequest)(
                  request.body
                ) match {
                case Right(message) =>
                  StatusTransition.transition(request.departure.status, MessageReceivedEvent.DeclarationCancellationRequest) match {
                    case Right(status) =>
                      submitMessageService
                        .submitMessage(departureId, request.departure.nextMessageId, message, status, request.channel)
                        .map {
                          case SubmissionProcessingResult.SubmissionFailureInternal =>
                            InternalServerError
                          case SubmissionProcessingResult.SubmissionFailureExternal =>
                            BadGateway
                          case submissionFailureRejected: SubmissionProcessingResult.SubmissionFailureRejected =>
                            BadRequest(submissionFailureRejected.responseBody)
                          case SubmissionProcessingResult.SubmissionSuccess =>
                            auditService.auditEvent(DepartureCancellationRequestSubmitted, message, request.channel)
                            Accepted
                              .withHeaders(
                                "Location" -> routes.MessagesController.getMessage(request.departure.departureId, request.departure.nextMessageId).url
                              )
                        }
                    case Left(error) =>
                      Future.successful(BadRequest(error.reason))
                  }
                case Left(error) =>
                  logger.warn(error.message)
                  Future.successful(BadRequest(error.message))
              }
            case _ =>
              Future.successful(NotImplemented)
          }
      }

    }

  def getMessages(departureId: DepartureId, receivedSince: Option[OffsetDateTime]): Action[AnyContent] =
    withMetricsTimerAction("get-all-departure-messages") {
      authenticateForRead(departureId) {
        implicit request =>
          val response = ResponseDepartureWithMessages.build(request.departure, receivedSince)
          messagesCount.update(response.messages.length)
          Ok(Json.toJsObject(response))
      }
    }

  def getMessage(departureId: DepartureId, messageId: MessageId): Action[AnyContent] =
    withMetricsTimerAction("get-departure-message") {
      authenticateForRead(departureId) {
        implicit request =>
          val messages = request.departure.messages.toList

          if (messages.isDefinedAt(messageId.index) && messages(messageId.index).optStatus != Some(SubmissionFailed))
            Ok(Json.toJsObject(ResponseMessage.build(departureId, messageId, messages(messageId.index))))
          else NotFound
      }
    }
}
