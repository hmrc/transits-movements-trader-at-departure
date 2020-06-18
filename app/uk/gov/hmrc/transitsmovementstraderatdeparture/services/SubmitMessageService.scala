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

package uk.gov.hmrc.transitsmovementstraderatdeparture.services

import java.time.OffsetDateTime

import javax.inject.Inject
import play.api.Logger
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.transitsmovementstraderatdeparture.connectors.MessageConnector
import uk.gov.hmrc.transitsmovementstraderatdeparture.models.{Departure, DepartureId, DepartureStatus, Message, MessageStatus, MessageWithStatus, SubmissionProcessingResult}
import uk.gov.hmrc.transitsmovementstraderatdeparture.repositories.DepartureRepository
import cats.implicits._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class SubmitMessageService @Inject()(departureRepository: DepartureRepository, messageConnector: MessageConnector)(implicit ec: ExecutionContext) {

  def submitMessage(departureId: DepartureId, messageId: Int, message: MessageWithStatus, departureStatus: DepartureStatus)(
    implicit hc: HeaderCarrier): Future[SubmissionProcessingResult] =
    departureRepository.addNewMessage(departureId, message) flatMap {
      case Failure(_) =>
        Future.successful(SubmissionProcessingResult.SubmissionFailureInternal)

      case Success(_) => {
        println("going in")
        messageConnector
          .post(departureId, message, OffsetDateTime.now)
          .flatMap {
            _ =>
              departureRepository
                .setDepartureStateAndMessageState(departureId, messageId, departureStatus, MessageStatus.SubmissionSucceeded)
                .map {
                  _ =>
                    SubmissionProcessingResult.SubmissionSuccess
                }
                .recover({
                  case _ =>
                    SubmissionProcessingResult.SubmissionFailureInternal
                })
          }
          .recoverWith {
            case error => {
              Logger.warn(s"Existing Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

              departureRepository.setMessageState(departureId,
                messageId,
                message.status.transition(SubmissionProcessingResult.SubmissionFailureInternal)) map {
                _ =>
                  SubmissionProcessingResult.SubmissionFailureExternal
              }
            }
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
            .post(departure.departureId, message, OffsetDateTime.now)
            .flatMap {
              _ =>
                departureRepository
                  .setDepartureStateAndMessageState(departure.departureId, messageId.index, DepartureStatus.DepartureSubmitted, MessageStatus.SubmissionSucceeded)
                  .map {
                    _ =>
                      SubmissionProcessingResult.SubmissionSuccess
                  }
                  .recover({
                    case _ =>
                      SubmissionProcessingResult.SubmissionFailureInternal
                  })
            }
            .recoverWith {
              case error =>
                Logger.warn(s"New Movement - Call to EIS failed with the following Exception: ${error.getMessage}")

                departureRepository.setMessageState(departure.departureId,
                  messageId.index,
                  message.status.transition(SubmissionProcessingResult.SubmissionFailureInternal)) map {
                  _ =>
                    SubmissionProcessingResult.SubmissionFailureExternal
                }
            }

      }
      .recover {
        case _ =>
          SubmissionProcessingResult.SubmissionFailureInternal
      }
}
