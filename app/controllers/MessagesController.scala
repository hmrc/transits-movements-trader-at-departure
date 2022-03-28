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

package controllers

import audit.AuditService
import audit.AuditType.DepartureCancellationRequestSubmitted
import cats.data.OptionT
import com.kenshoo.play.metrics.Metrics
import controllers.actions.AuthenticateActionProvider
import controllers.actions.AuthenticatedGetDepartureWithMessagesForReadActionProvider
import controllers.actions.AuthenticatedGetDepartureWithoutMessagesForWriteActionProvider
import metrics.HasActionMetrics
import models.MessageStatus.SubmissionFailed
import models._
import models.request.DepartureWithoutMessagesRequest
import models.response.ResponseDepartureWithMessages
import models.response.ResponseMessage
import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.ControllerComponents
import repositories.DepartureRepository
import services.DepartureService
import services.SubmitMessageService
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import java.time.OffsetDateTime
import javax.inject.Inject
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class MessagesController @Inject() (
  cc: ControllerComponents,
  authenticate: AuthenticateActionProvider,
  authenticateForReadWithMessages: AuthenticatedGetDepartureWithMessagesForReadActionProvider,
  authenticateForWriteWithoutMessages: AuthenticatedGetDepartureWithoutMessagesForWriteActionProvider,
  departureService: DepartureService,
  auditService: AuditService,
  submitMessageService: SubmitMessageService,
  departureRepository: DepartureRepository,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  lazy val messagesCount = histo("get-all-departure-messages-count")

  def post(departureId: DepartureId): Action[NodeSeq] =
    withMetricsTimerAction("post-submit-message") {
      authenticateForWriteWithoutMessages(departureId).async(parse.xml) {
        implicit request: DepartureWithoutMessagesRequest[NodeSeq] =>
          MessageType.getMessageType(request.body) match {
            case Some(MessageType.DeclarationCancellationRequest) =>
              departureService
                .makeMessageWithStatus(
                  request.departure.departureId,
                  request.departure.nextMessageId,
                  request.departure.nextMessageCorrelationId,
                  MessageType.DeclarationCancellationRequest
                )(
                  request.body
                ) match {
                case Right(message) =>
                  submitMessageService
                    .submitMessage(departureId, message, request.channel)
                    .map {
                      case SubmissionProcessingResult.SubmissionFailureInternal =>
                        InternalServerError
                      case SubmissionProcessingResult.SubmissionFailureExternal =>
                        BadGateway
                      case submissionFailureRejected: SubmissionProcessingResult.SubmissionFailureRejected =>
                        BadRequest(submissionFailureRejected.responseBody)
                      case SubmissionProcessingResult.SubmissionSuccess =>
                        auditService.auditEvent(
                          DepartureCancellationRequestSubmitted,
                          request.request.enrolmentId.customerId,
                          request.request.enrolmentId.enrolmentType,
                          message,
                          request.channel
                        )
                        Accepted
                          .withHeaders(
                            "Location" -> routes.MessagesController.getMessage(request.departure.departureId, request.departure.nextMessageId).url
                          )
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
      authenticateForReadWithMessages(departureId) {
        implicit request =>
          val response = ResponseDepartureWithMessages.build(request.departure, receivedSince)
          messagesCount.update(response.messages.length)
          Ok(Json.toJsObject(response))
      }
    }

  @nowarn("msg=parameter value departure in anonymous function is never used")
  def getMessage(departureId: DepartureId, messageId: MessageId): Action[AnyContent] =
    withMetricsTimerAction("get-departure-message") {
      authenticate().async {
        implicit request =>
          val result = for {
            departure <- OptionT(departureRepository.getWithoutMessages(departureId, request.channel))
            if request.hasMatchingEnrolmentId(departure)
            message <- OptionT(departureRepository.getMessage(departureId, request.channel, messageId))
            if !message.optStatus.contains(SubmissionFailed)
          } yield Ok(Json.toJsObject(ResponseMessage.build(departureId, message)))

          result.getOrElse(NotFound)
      }

    }
}
