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

import cats.implicits._
import connectors.MessageConnector
import connectors.MessageConnector.EisSubmissionResult
import connectors.MessageConnector.EisSubmissionResult._
import models._
import play.api.Logging
import repositories.DepartureRepository
import uk.gov.hmrc.http.HeaderCarrier

import java.time.Clock
import java.time.OffsetDateTime
import java.util.concurrent.TimeoutException
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

class SubmitMessageService @Inject()(departureRepository: DepartureRepository, messageConnector: MessageConnector)(implicit clock: Clock, ec: ExecutionContext)
    extends Logging {

  def submitMessage(departureId: DepartureId, message: MessageWithStatus, channelType: ChannelType)(
    implicit hc: HeaderCarrier
  ): Future[SubmissionProcessingResult] =
    departureRepository
      .addNewMessage(departureId, message)
      .flatMap {
        case Failure(_) =>
          Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)
        case Success(_) =>
          submitToEis(departureId, message, channelType)("submitMessage")
      }

  def submitDeparture(departure: Departure)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    departureRepository
      .insert(departure)
      .flatMap {
        _ =>
          val (message, _) = departure.messagesWithId.head.leftMap(_.asInstanceOf[MessageWithStatus])
          submitToEis(departure.departureId, message, departure.channel)("submitDeparture")
      }
      .recover {
        case NonFatal(e) =>
          logger.error("Mongo failure when inserting a new departure", e)
          SubmissionProcessingResult.SubmissionFailureInternal
      }

  private def submitToEis(
    departureId: DepartureId,
    message: MessageWithStatus,
    channel: ChannelType
  )(method: String)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    messageConnector
      .post(departureId, message, OffsetDateTime.now, channel)
      .flatMap {
        case EisSubmissionSuccessful =>
          updateDepartureAndMessage(departureId, message.messageId)

        case submissionResult: EisSubmissionRejected =>
          logger.warn(s"Failure for $method of type: ${message.messageType.code}, and details: ${submissionResult.toString}")
          updateMessage(departureId, message, submissionResult)(_ =>
            submissionResult match {
              case ErrorInPayload =>
                SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
              case VirusFoundOrInvalidToken =>
                SubmissionProcessingResult.SubmissionFailureInternal
          })(SubmissionProcessingResult.SubmissionFailureInternal)

        case submissionResult: EisSubmissionFailureDownstream =>
          logger.warn(s"Failure for $method of type: ${message.messageType.code}, and details: ${submissionResult.toString}")
          updateMessage(departureId, message, submissionResult)(
            _ => SubmissionProcessingResult.SubmissionFailureExternal
          )(SubmissionProcessingResult.SubmissionFailureExternal)
      }
      .recoverWith {
        case e: TimeoutException =>
          logger.error("EIS submission timed out", e)
          updateMessage(departureId, message, DownstreamGatewayTimeout)(
            _ => SubmissionProcessingResult.SubmissionFailureExternal
          )(SubmissionProcessingResult.SubmissionFailureExternal)
      }

  private def updateMessage(
    departureId: DepartureId,
    message: MessageWithStatus,
    submissionResult: EisSubmissionResult
  )(
    processResult: Try[Unit] => SubmissionProcessingResult
  )(
    defaultResult: SubmissionProcessingResult
  ): Future[SubmissionProcessingResult] = {
    val selector = MessageSelector(departureId, message.messageId)
    val modifier = MessageStatusUpdate(message.messageId, message.status.transition(submissionResult))
    updateDeparture(selector, modifier)(processResult)(defaultResult)
  }

  private def updateDepartureAndMessage(
    departureId: DepartureId,
    messageId: MessageId,
    messageState: MessageStatus = MessageStatus.SubmissionSucceeded
  ): Future[SubmissionProcessingResult] = {
    val selector = DepartureIdSelector(departureId)
    val modifier = MessageStatusUpdate(messageId, messageState)
    updateDeparture(selector, modifier)(
      _ => SubmissionProcessingResult.SubmissionSuccess
    )(SubmissionProcessingResult.SubmissionFailureInternal)
  }

  private def updateDeparture(
    selector: DepartureSelector,
    modifier: MessageStatusUpdate
  )(
    processResult: Try[Unit] => SubmissionProcessingResult
  )(
    defaultResult: SubmissionProcessingResult
  ): Future[SubmissionProcessingResult] =
    departureRepository
      .updateDeparture(selector, modifier)
      .map(processResult)
      .recover({
        case NonFatal(e) =>
          logger.error("Mongo failure when updating message status", e)
          defaultResult
      })
}
