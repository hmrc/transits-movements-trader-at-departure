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

import audit.AuditService
import controllers.actions.CheckMessageTypeActionProvider
import controllers.actions.GetDepartureForWriteActionProvider

import javax.inject.Inject
import models.MessageSender
import models.MessageType
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.ControllerComponents
import services.SaveMessageService
import services.XmlMessageParser
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.xml.NodeSeq

class NCTSMessageController @Inject()(cc: ControllerComponents,
                                      getDeparture: GetDepartureForWriteActionProvider,
                                      checkMessageType: CheckMessageTypeActionProvider,
                                      auditService: AuditService,
                                      saveMessageService: SaveMessageService)(implicit ec: ExecutionContext)
    extends BackendController(cc) {

  def post(messageSender: MessageSender): Action[NodeSeq] = (getDeparture(messageSender.departureId) andThen checkMessageType())(parse.xml).async {
    implicit request =>
      val xml: NodeSeq = request.request.body
      val response     = request.messageResponse

      request.departure.status.transition(response.messageReceived) match {
        case Right(newState) =>
          response.messageType match {
            case MessageType.MrnAllocated =>
              XmlMessageParser.mrnR(xml) match {
                case Left(error) =>
                  Logger.warn(error.message)
                  Future.successful(BadRequest(error.message))
                case Right(mrn) =>
                  val processingResult = saveMessageService.validateXmlSaveMessageUpdateMrn(xml, messageSender, response, newState, mrn)
                  processingResult map {
                    case SubmissionSuccess =>
                      auditService.auditNCTSMessages(request.request.departure.channel, response, xml)
                      Ok.withHeaders(
                        LOCATION -> routes.MessagesController.getMessage(request.request.departure.departureId, request.request.departure.nextMessageId).url)
                    case SubmissionFailureInternal =>
                      val message = "Internal Submission Failure " + processingResult
                      Logger.warn(message)
                      InternalServerError
                    case SubmissionFailureExternal =>
                      val message = "External Submission Failure " + processingResult
                      Logger.warn(message)
                      BadRequest
                  }
              }
            case _ =>
              val processingResult = saveMessageService.validateXmlAndSaveMessage(xml, messageSender, response, newState)
              processingResult map {
                case SubmissionSuccess =>
                  auditService.auditNCTSMessages(request.request.departure.channel, response, xml)
                  Ok.withHeaders(
                    LOCATION -> routes.MessagesController.getMessage(request.request.departure.departureId, request.request.departure.nextMessageId).url)
                case SubmissionFailureInternal =>
                  val message = "Internal Submission Failure " + processingResult
                  Logger.warn(message)
                  InternalServerError
                case SubmissionFailureExternal =>
                  val message = "External Submission Failure " + processingResult
                  Logger.warn(message)
                  BadRequest
              }
          }
        case Left(error) =>
          Future.successful(BadRequest(error.reason))
      }
  }
}
