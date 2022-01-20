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
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class SubmitMessageService @Inject()(departureRepository: DepartureRepository, messageConnector: MessageConnector)(implicit clock: Clock, ec: ExecutionContext)
    extends Logging {

  def submitMessage(departureId: DepartureId, message: MessageWithStatus, departureStatus: DepartureStatus, channelType: ChannelType)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    departureRepository.addNewMessage(departureId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)

      case Success(_) =>
        messageConnector
          .post(departureId, message, OffsetDateTime.now, channelType)
          .flatMap {
            submissionResult =>
              submissionResult match {
                case EisSubmissionSuccessful =>
                  val newStatus = message.status.transition(submissionResult)

                  updateDepartureAndMessage(departureId, message.messageId, departureStatus, newStatus)
                    .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                    .recover({
                      case _ =>
                        // TODO: Can this recover be moved to the repository layer.
                        //  Encode the exception in the failed Future that Reactive Mongo returns as an ADT
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionRejected =>
                  logger.warn(s"Failure for submitMessage of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  updateMessage(departureId, message, submissionResult)
                    .map(_ =>
                      submissionResult match {
                        case ErrorInPayload =>
                          SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                        case VirusFoundOrInvalidToken =>
                          SubmissionProcessingResult.SubmissionFailureInternal
                    })
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureInternal
                    })

                case submissionResult: EisSubmissionFailureDownstream =>
                  logger.warn(s"Failure for submitMessage of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                  updateMessage(departureId, message, submissionResult)
                    .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                    .recover({
                      case _ =>
                        logger.warn("Mongo failure when updating message status")
                        SubmissionProcessingResult.SubmissionFailureExternal
                    })
              }
          }
    }

  def submitDeparture(departure: Departure)(implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    departureRepository
      .insert(departure)
      .flatMap {
        _ =>
          val (message, messageId) = departure.messagesWithId.head.leftMap(_.asInstanceOf[MessageWithStatus])

          messageConnector
            .post(departure.departureId, message, OffsetDateTime.now, departure.channel)
            .flatMap {
              case EisSubmissionSuccessful =>
                updateDepartureAndMessage(departure.departureId, messageId)
                  .map(_ => SubmissionProcessingResult.SubmissionSuccess)
                  .recover({
                    case _ =>
                      logger.warn("Mongo failure when updating message status")
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })

              case submissionResult: EisSubmissionRejected =>
                logger.warn(s"Failure for submitDeparture of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                updateMessage(departure.departureId, message, submissionResult)
                  .map(_ =>
                    submissionResult match {
                      case ErrorInPayload =>
                        SubmissionProcessingResult.SubmissionFailureRejected(submissionResult.responseBody)
                      case VirusFoundOrInvalidToken =>
                        SubmissionProcessingResult.SubmissionFailureInternal
                  })
                  .recover({
                    case _ =>
                      logger.warn("Mongo failure when updating message status")
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })

              case submissionResult: EisSubmissionFailureDownstream =>
                logger.warn(s"Failure for submitDeparture of type: ${message.messageType.code}, and details: " + submissionResult.toString)

                updateMessage(departure.departureId, message, submissionResult)
                  .map(_ => SubmissionProcessingResult.SubmissionFailureExternal)
                  .recover({
                    case _ =>
                      logger.warn("Mongo failure when updating message status")
                      SubmissionProcessingResult.SubmissionFailureExternal
                  })
            }

      }
      .recover {
        case _ =>
          logger.warn("Mongo failure when inserting a new departure")
          SubmissionProcessingResult.SubmissionFailureInternal
      }

  private def updateMessage(
    departureId: DepartureId,
    message: MessageWithStatus,
    submissionResult: EisSubmissionResult
  ): Future[Try[Unit]] = {
    val selector = MessageSelector(departureId, message.messageId)
    val modifier = MessageStatusUpdate(message.messageId, message.status.transition(submissionResult))

    departureRepository.updateDeparture(selector, modifier)
  }

  private def updateDepartureAndMessage(
    departureId: DepartureId,
    messageId: MessageId,
    departureState: DepartureStatus = DepartureStatus.DepartureSubmitted,
    messageState: MessageStatus = MessageStatus.SubmissionSucceeded
  ): Future[Try[Unit]] = {
    val selector = DepartureIdSelector(departureId)
    val modifier = CompoundStatusUpdate(DepartureStatusUpdate(departureState), MessageStatusUpdate(messageId, messageState))

    departureRepository.updateDeparture(selector, modifier)
  }
}
