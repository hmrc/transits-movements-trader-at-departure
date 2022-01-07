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

package services

import com.google.inject.Inject
import models.SubmissionProcessingResult.SubmissionFailureExternal
import models.SubmissionProcessingResult.SubmissionFailureInternal
import models.SubmissionProcessingResult.SubmissionSuccess
import models._
import play.api.Logging
import repositories.DepartureRepository

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.xml.NodeSeq

class SaveMessageService @Inject()(departureRepository: DepartureRepository, departureService: DepartureService, xmlValidationService: XmlValidationService)(
  implicit ec: ExecutionContext
) extends Logging {

  def validateXmlAndSaveMessage(
    nextMessageId: MessageId,
    messageXml: NodeSeq,
    messageSender: MessageSender,
    messageResponse: MessageResponse
  ): Future[SubmissionProcessingResult] =
    xmlValidationService.validate(messageXml.toString(), messageResponse.xsdFile) match {
      case Success(_) =>
        departureService.makeMessage(nextMessageId, messageSender.messageCorrelationId, messageResponse.messageType)(messageXml) match {
          case Right(message) =>
            departureRepository
              .addResponseMessage(messageSender.departureId, message)
              .map {
                case Success(_) => SubmissionSuccess
                case Failure(_) => SubmissionFailureInternal
              }
          case Left(_) => Future.successful(SubmissionFailureExternal)
        }
      case Failure(e) =>
        logger.warn(s"Failure to validate against XSD. Exception: ${e.getMessage}")
        Future.successful(SubmissionFailureExternal)
    }

  def validateXmlSaveMessageUpdateMrn(
    nextMessageId: MessageId,
    messageXml: NodeSeq,
    messageSender: MessageSender,
    messageResponse: MessageResponse,
    mrn: MovementReferenceNumber
  ): Future[SubmissionProcessingResult] =
    xmlValidationService.validate(messageXml.toString(), messageResponse.xsdFile) match {
      case Success(_) =>
        departureService.makeMessage(nextMessageId, messageSender.messageCorrelationId, messageResponse.messageType)(messageXml) match {
          case Right(message) =>
            departureRepository
              .setMrnAndAddResponseMessage(messageSender.departureId, message, mrn)
              .map {
                case Success(_) => SubmissionSuccess
                case Failure(_) => SubmissionFailureInternal
              }
          case Left(_) => Future.successful(SubmissionFailureExternal)
        }
      case Failure(e) =>
        logger.warn(s"Failure to validate against XSD. Exception: ${e.getMessage}")
        Future.successful(SubmissionFailureExternal)
    }
}
