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

package models

import models.DepartureStatus._

object StatusTransition {

  def transitionError(
    currentStatus: DepartureStatus,
    requiredStatuses: Set[DepartureStatus],
    targetStatus: Option[DepartureStatus],
    messageReceived: MessageReceivedEvent
  ): Either[TransitionError, DepartureStatus] = {
    val messageType = messageReceived.toString

    val requiredStatusesString = requiredStatuses
      .filterNot(targetStatus.contains)
      .map(_.toString)
      .toList
      .sorted
      .mkString(" or ")

    Left(
      TransitionError(
        s"Can only accept this type of message [$messageType] directly after [$requiredStatusesString] messages. Current message state is [${currentStatus.toString}]."
      )
    )
  }

  def targetStatus(currentStatus: DepartureStatus, messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = messageReceived match {
    case MessageReceivedEvent.CancellationDecision           => Right(CancellationDecision)
    case MessageReceivedEvent.ControlDecisionNotification    => Right(ControlDecisionNotification)
    case MessageReceivedEvent.DeclarationCancellationRequest => Right(DeclarationCancellationRequest)
    case MessageReceivedEvent.DepartureRejected              => Right(DepartureRejected)
    case MessageReceivedEvent.DepartureSubmitted             => Right(DepartureSubmitted)
    case MessageReceivedEvent.GuaranteeNotValid              => Right(GuaranteeNotValid)
    case MessageReceivedEvent.MrnAllocated                   => Right(MrnAllocated)
    case MessageReceivedEvent.NoReleaseForTransit            => Right(NoReleaseForTransit)
    case MessageReceivedEvent.PositiveAcknowledgement        => Right(PositiveAcknowledgement)
    case MessageReceivedEvent.ReleaseForTransit              => Right(ReleaseForTransit)
    case MessageReceivedEvent.WriteOffNotification           => Right(WriteOffNotification)
    case MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement =>
      currentStatus match {
        case Initialized =>
          Right(DepartureSubmittedNegativeAcknowledgement)
        case DepartureSubmitted =>
          Right(DepartureSubmittedNegativeAcknowledgement)
        case DeclarationCancellationRequest =>
          Right(DeclarationCancellationRequestNegativeAcknowledgement)
        case DepartureSubmittedNegativeAcknowledgement =>
          Right(DepartureSubmittedNegativeAcknowledgement)
        case DeclarationCancellationRequestNegativeAcknowledgement =>
          Right(DeclarationCancellationRequestNegativeAcknowledgement)
        case _ =>
          Right(currentStatus)
      }
  }
}
