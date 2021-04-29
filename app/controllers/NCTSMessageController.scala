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
import com.kenshoo.play.metrics.Metrics
import controllers.actions.CheckMessageTypeActionProvider
import controllers.actions.GetDepartureForWriteActionProvider
import logging.Logging
import metrics.HasActionMetrics
import models.MessageSender
import models.MessageType
import models.StatusTransition
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services.SaveMessageService
import services.XmlMessageParser
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class NCTSMessageController @Inject() (
  cc: ControllerComponents,
  getDeparture: GetDepartureForWriteActionProvider,
  checkMessageType: CheckMessageTypeActionProvider,
  auditService: AuditService,
  saveMessageService: SaveMessageService,
  val metrics: Metrics
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging
    with HasActionMetrics {

  def post(messageSender: MessageSender): Action[NodeSeq] =
    withMetricsTimerAction("post-receive-ncts-message") {
      (getDeparture(messageSender.departureId) andThen checkMessageType())(parse.xml).async {
        implicit request =>
          val xml: NodeSeq = request.request.body
          val response     = request.messageResponse

          StatusTransition.transition(request.departure.status, response.messageReceived) match {
            case Right(newState) =>
              response.messageType match {
                case MessageType.MrnAllocated =>
                  XmlMessageParser.mrnR(xml) match {
                    case Left(error) =>
                      logger.warn(error.message)
                      Future.successful(BadRequest(error.message))
                    case Right(mrn) =>
                      val processingResult = saveMessageService.validateXmlSaveMessageUpdateMrn(xml, messageSender, response, newState, mrn)
                      processingResult map {
                        case SubmissionSuccess =>
                          auditService.auditNCTSMessages(request.request.departure.channel, response, xml)
                          Ok.withHeaders(
                            LOCATION -> routes.MessagesController.getMessage(request.request.departure.departureId, request.request.departure.nextMessageId).url
                          )
                        case SubmissionFailureInternal =>
                          val message = "Internal Submission Failure " + processingResult
                          logger.warn(message)
                          InternalServerError
                        case SubmissionFailureExternal =>
                          val message = "External Submission Failure " + processingResult
                          logger.warn(message)
                          BadRequest
                      }
                  }
                case _ =>
                  val processingResult = saveMessageService.validateXmlAndSaveMessage(xml, messageSender, response, newState)
                  processingResult map {
                    case SubmissionSuccess =>
                      auditService.auditNCTSMessages(request.request.departure.channel, response, xml)
                      Ok.withHeaders(
                        LOCATION -> routes.MessagesController.getMessage(request.request.departure.departureId, request.request.departure.nextMessageId).url
                      )
                    case SubmissionFailureInternal =>
                      val message = "Internal Submission Failure " + processingResult
                      logger.warn(message)
                      InternalServerError
                    case SubmissionFailureExternal =>
                      val message = "External Submission Failure " + processingResult
                      logger.warn(message)
                      BadRequest
                  }
              }
            case Left(error) =>
              Future.successful(BadRequest(error.reason))
          }
      }

    }
}
