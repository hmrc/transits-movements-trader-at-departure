/*
 * Copyright 2023 HM Revenue & Customs
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

package utils

import models.DepartureStatus
import models.MessageType
import models.MessageTypeWithTime
import play.api.Logging

import java.time.LocalDateTime
import scala.annotation.tailrec

object MessageTypeUtils extends Logging {

  private def getMessage(
    messages: List[MessageTypeWithTime],
    drop: Int
  )(implicit mto: Ordering[MessageType]): Option[MessageTypeWithTime] = {
    implicit val ldto: Ordering[LocalDateTime] = _ compareTo _
    messages
      .sortBy(
        m => (m.dateTime, m.messageType)
      )
      .dropRight(drop)
      .lastOption
  }

  private def getLatestMessage(messages: List[MessageTypeWithTime]): Option[MessageTypeWithTime] =
    getMessage(messages, 0)(MessageType.latestMessageOrdering)

  private def getPreviousMessage(messages: List[MessageTypeWithTime]): Option[MessageTypeWithTime] =
    getMessage(messages, 1)(MessageType.previousMessageOrdering)

  @tailrec
  def latestDepartureStatus(messages: List[MessageTypeWithTime]): DepartureStatus =
    getLatestMessage(messages) match {
      case Some(latestMessage) =>
        latestMessage.messageType match {
          case MessageType.PositiveAcknowledgement        => DepartureStatus.PositiveAcknowledgement
          case MessageType.DepartureDeclaration           => DepartureStatus.DepartureSubmitted
          case MessageType.MrnAllocated                   => DepartureStatus.MrnAllocated
          case MessageType.DeclarationRejected            => DepartureStatus.DepartureRejected
          case MessageType.ControlDecisionNotification    => DepartureStatus.ControlDecisionNotification
          case MessageType.NoReleaseForTransit            => DepartureStatus.NoReleaseForTransit
          case MessageType.ReleaseForTransit              => DepartureStatus.ReleaseForTransit
          case MessageType.DeclarationCancellationRequest => DepartureStatus.DeclarationCancellationRequest
          case MessageType.CancellationDecision           => DepartureStatus.CancellationDecision
          case MessageType.WriteOffNotification           => DepartureStatus.WriteOffNotification
          case MessageType.GuaranteeNotValid              => DepartureStatus.GuaranteeNotValid
          case MessageType.XMLSubmissionNegativeAcknowledgement =>
            logger.info("[latestDepartureStatus] Latest message is of type XMLSubmissionNegativeAcknowledgement. Checking previous message.")
            getPreviousMessage(messages) match {
              case Some(previousMessage) =>
                previousMessage.messageType match {
                  case MessageType.DepartureDeclaration           => DepartureStatus.DepartureSubmittedNegativeAcknowledgement
                  case MessageType.DeclarationCancellationRequest => DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
                  case _                                          => latestDepartureStatus(messages.filterNot(_ == latestMessage))
                }
              case None => DepartureStatus.Undetermined
            }
        }
      case None => DepartureStatus.Undetermined
    }
}
