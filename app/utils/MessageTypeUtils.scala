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

import scala.annotation.tailrec

object MessageTypeUtils {

  implicit private val localDateOrdering: Ordering[LocalDateTime] = _ compareTo _

  private def getLatestMessage(messagesList: List[MessageTypeWithTime]): MessageTypeWithTime = {
    val latestMessage            = messagesList.maxBy(_.dateTime)
    val messagesWithSameDateTime = messagesList.filter(_.dateTime == latestMessage.dateTime)

    if (messagesWithSameDateTime.size == 1) latestMessage else messagesWithSameDateTime.maxBy(_.messageType)
  }

  private def getPreviousMessage(messagesList: List[MessageTypeWithTime]): MessageTypeWithTime = {
    val previousMessage               = messagesList.sortBy(_.dateTime).takeRight(2).head
    lazy val messagesWithSameDateTime = messagesList.filter(_.dateTime == previousMessage.dateTime)

    previousMessage.messageType match {
      case MessageType.DepartureDeclaration | MessageType.DeclarationCancellationRequest => previousMessage
      case _ if messagesWithSameDateTime.size == 1                                       => previousMessage
      case _                                                                             => messagesWithSameDateTime.maxBy(_.messageType)
    }
  }

  @tailrec
  def latestDepartureStatus(messagesList: List[MessageTypeWithTime]): DepartureStatus = {
    val latestMessage = getLatestMessage(messagesList)
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
        getPreviousMessage(messagesList).messageType match {
          case MessageType.DepartureDeclaration           => DepartureStatus.DepartureSubmittedNegativeAcknowledgement
          case MessageType.DeclarationCancellationRequest => DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
          case _                                          => latestDepartureStatus(messagesList.filterNot(_ == latestMessage))
        }
    }
  }
}
