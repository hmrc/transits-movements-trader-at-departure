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

package utils

import java.time.LocalDateTime

import models.DepartureStatus
import models.MessageType
import models.MessageTypeWithTime

object MessageTypeUtils {

  def currentDepartureStatus(messagesList: List[MessageTypeWithTime]): DepartureStatus = {
    implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

    val latestMessage            = messagesList.maxBy(_.dateTime)
    val messagesWithSameDateTime = messagesList.filter(_.dateTime == latestMessage.dateTime)

    val currentMessageType = if (messagesWithSameDateTime.size == 1) {
      latestMessage.messageType
    } else {
      messagesWithSameDateTime.map(_.messageType).max
    }
    toDepartureStatus(currentMessageType)
  }

  def previousDepartureStatus(messagesList: List[MessageTypeWithTime], currentStatus: DepartureStatus): DepartureStatus = {

    implicit val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

    val previousMessage = messagesList.sortBy(_.dateTime).takeRight(2).head

    val messagesWithSameDateTime = messagesList.filter(_.dateTime == previousMessage.dateTime)

    val previousMessageType = if (messagesWithSameDateTime.size == 1) {
      previousMessage.messageType
    } else {
      currentStatus match {
        case DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement =>
          if (previousMessage.messageType == MessageType.DepartureDeclaration || previousMessage.messageType == MessageType.DeclarationCancellationRequest) {
            previousMessage.messageType
          } else {
            messagesWithSameDateTime.map(_.messageType).max
          }
        case _ => messagesWithSameDateTime.map(_.messageType).max
      }
    }

    toDepartureStatus(previousMessageType)
  }

  def toDepartureStatus(messageType: MessageType): DepartureStatus =
    messageType match {
      case MessageType.PositiveAcknowledgement              => DepartureStatus.PositiveAcknowledgement
      case MessageType.DepartureDeclaration                 => DepartureStatus.DepartureSubmitted
      case MessageType.MrnAllocated                         => DepartureStatus.MrnAllocated
      case MessageType.DeclarationRejected                  => DepartureStatus.DepartureRejected
      case MessageType.ControlDecisionNotification          => DepartureStatus.ControlDecisionNotification
      case MessageType.NoReleaseForTransit                  => DepartureStatus.NoReleaseForTransit
      case MessageType.ReleaseForTransit                    => DepartureStatus.ReleaseForTransit
      case MessageType.DeclarationCancellationRequest       => DepartureStatus.DeclarationCancellationRequest
      case MessageType.CancellationDecision                 => DepartureStatus.CancellationDecision
      case MessageType.WriteOffNotification                 => DepartureStatus.WriteOffNotification
      case MessageType.GuaranteeNotValid                    => DepartureStatus.GuaranteeNotValid
      case MessageType.XMLSubmissionNegativeAcknowledgement => DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
    }
}
