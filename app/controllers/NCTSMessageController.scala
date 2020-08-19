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

import controllers.actions.GetDepartureForWriteActionProvider
import javax.inject.Inject
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import models.CancellationDecisionResponse
import models.ControlDecisionNotificationResponse
import models.DepartureRejectedResponse
import models.MessageResponse
import models.MessageSender
import models.MessageType
import models.MrnAllocatedResponse
import models.NoReleaseForTransitResponse
import models.PositiveAcknowledgementResponse
import models.ReleaseForTransitResponse
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services.SaveMessageService
import services.XmlMessageParser
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class NCTSMessageController @Inject()(cc: ControllerComponents, getDeparture: GetDepartureForWriteActionProvider, saveMessageService: SaveMessageService)(
  implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post(messageSender: MessageSender): Action[NodeSeq] = getDeparture(messageSender.departureId)(parse.xml).async {
    implicit request =>
      val xml: NodeSeq = request.request.body

      val messageResponse: Option[MessageResponse] = request.headers.get("X-Message-Type") match {
        case Some(MessageType.PositiveAcknowledgement.code)     => Some(PositiveAcknowledgementResponse)
        case Some(MessageType.MrnAllocated.code)                => Some(MrnAllocatedResponse)
        case Some(MessageType.DeclarationRejected.code)         => Some(DepartureRejectedResponse)
        case Some(MessageType.ControlDecisionNotification.code) => Some(ControlDecisionNotificationResponse)
        case Some(MessageType.NoReleaseForTransit.code)         => Some(NoReleaseForTransitResponse)
        case Some(MessageType.ReleaseForTransit.code)           => Some(ReleaseForTransitResponse)
        case Some(MessageType.CancellationDecision.code)        => Some(CancellationDecisionResponse)
        case invalidResponse =>
          Logger.warn(s"Received the following invalid response for X-Message-Type: $invalidResponse")
          None
      }

      messageResponse match {
        case Some(response) =>
          val newState = request.departure.status.transition(response.messageReceived)
          response.messageType match {
            case MessageType.MrnAllocated =>
              XmlMessageParser.mrnR(xml) match {
                case None =>
                  val message = "Missing MRN"
                  Logger.warn(message)
                  Future.successful(BadRequest(message))
                case Some(mrn) =>
                  val processingResult = saveMessageService.validateXmlSaveMessageUpdateMrn(xml, messageSender, response, newState, mrn)
                  processingResult map {
                    case SubmissionSuccess => Ok
                    case SubmissionFailureInternal =>
                      val message = "Internal Submission Failure " + processingResult
                      Logger.warn(message)
                      InternalServerError(message)
                    case SubmissionFailureExternal =>
                      val message = "External Submission Failure " + processingResult
                      Logger.warn(message)
                      BadRequest(message)
                  }
              }
            case _ =>
              val processingResult = saveMessageService.validateXmlAndSaveMessage(xml, messageSender, response, newState)
              processingResult map {
                case SubmissionSuccess => Ok
                case SubmissionFailureInternal =>
                  val message = "Internal Submission Failure " + processingResult
                  Logger.warn(message)
                  InternalServerError(message)
                case SubmissionFailureExternal =>
                  val message = "External Submission Failure " + processingResult
                  Logger.warn(message)
                  BadRequest(message)
              }
          }
        case None =>
          val message = "No response from downstream NCTS";
          Logger.warn(message)
          Future.successful(BadRequest(message))
      }
  }
}
