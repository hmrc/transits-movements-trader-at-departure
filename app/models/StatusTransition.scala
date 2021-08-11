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

  val validTransitions = Set(
    // Initialized transitions
    Initialized -> DepartureSubmitted,
    Initialized -> MrnAllocated,
    Initialized -> DepartureRejected,
    Initialized -> PositiveAcknowledgement,
    Initialized -> DepartureSubmittedNegativeAcknowledgement,
    // DepartureSubmitted transitions
    DepartureSubmitted -> DepartureSubmitted,
    DepartureSubmitted -> PositiveAcknowledgement,
    DepartureSubmitted -> MrnAllocated,
    DepartureSubmitted -> DepartureRejected,
    DepartureSubmitted -> DepartureSubmittedNegativeAcknowledgement,
    // PositiveAcknowledgement transitions
    PositiveAcknowledgement -> PositiveAcknowledgement,
    PositiveAcknowledgement -> MrnAllocated,
    PositiveAcknowledgement -> DepartureRejected,
    PositiveAcknowledgement -> WriteOffNotification,
    // MrnAllocated transitions
    MrnAllocated -> MrnAllocated,
    MrnAllocated -> ControlDecisionNotification,
    MrnAllocated -> NoReleaseForTransit,
    MrnAllocated -> ReleaseForTransit,
    MrnAllocated -> DeclarationCancellationRequest,
    MrnAllocated -> GuaranteeNotValid,
    // DepartureRejected transitions
    DepartureRejected -> DepartureRejected,
    // ControlDecisionNotification transitions
    ControlDecisionNotification -> ControlDecisionNotification,
    ControlDecisionNotification -> NoReleaseForTransit,
    ControlDecisionNotification -> ReleaseForTransit,
    ControlDecisionNotification -> DeclarationCancellationRequest,
    // NoReleaseForTransit transitions
    NoReleaseForTransit -> NoReleaseForTransit,
    // ReleaseForTransit transitions
    ReleaseForTransit -> ReleaseForTransit,
    ReleaseForTransit -> CancellationDecision,
    ReleaseForTransit -> WriteOffNotification,
    // DeclarationCancellationRequest transitions
    DeclarationCancellationRequest -> DeclarationCancellationRequest,
    DeclarationCancellationRequest -> CancellationDecision,
    DeclarationCancellationRequest -> DeclarationCancellationRequestNegativeAcknowledgement,
    DeclarationCancellationRequest -> ReleaseForTransit,
    DeclarationCancellationRequest -> NoReleaseForTransit,
    // CancellationDecision transitions
    CancellationDecision -> CancellationDecision,
    CancellationDecision -> WriteOffNotification,
    // WriteOffNotification transitions
    WriteOffNotification -> WriteOffNotification,
    // GuaranteeNotValid transitions
    GuaranteeNotValid -> GuaranteeNotValid,
    GuaranteeNotValid -> NoReleaseForTransit,
    GuaranteeNotValid -> ReleaseForTransit,
    GuaranteeNotValid -> DeclarationCancellationRequest,
    // DepartureSubmittedNegativeAcknowledgement transitions
    DepartureSubmittedNegativeAcknowledgement -> DepartureSubmittedNegativeAcknowledgement,
    // DeclarationCancellationRequestNegativeAcknowledgement transitions
    DeclarationCancellationRequestNegativeAcknowledgement -> DeclarationCancellationRequestNegativeAcknowledgement,
    DeclarationCancellationRequestNegativeAcknowledgement -> DeclarationCancellationRequest
  )

  /** Mapping of the statuses we can transition to from a given status
    */
  val allowedStatusForTransition = validTransitions
    .groupBy {
      case (from, _) => from
    }
    .mapValues(_.map {
      case (_, to) => to
    })
    .toMap

  /** Mapping of the statuses that are able to transition to a given status
    */
  val requiredStatusForTransition = validTransitions
    .groupBy {
      case (_, to) => to
    }
    .mapValues(_.map {
      case (from, _) => from
    })
    .toMap

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
          transitionError(currentStatus, Set(Initialized, DepartureSubmitted, DeclarationCancellationRequest), None, messageReceived)
      }
  }

  def transition(currentStatus: DepartureStatus, messageReceived: MessageReceivedEvent): Either[SubmissionState, DepartureStatus] =
    targetStatus(currentStatus, messageReceived).flatMap {
      transitionToStatus =>
        val allowedFromThisStatus   = allowedStatusForTransition.get(currentStatus).getOrElse(Set.empty)
        val requiredForTargetStatus = requiredStatusForTransition.get(transitionToStatus).getOrElse(Set.empty)
        if (allowedFromThisStatus.contains(transitionToStatus))
          Right(transitionToStatus)
        else
          transitionError(currentStatus, requiredForTargetStatus, Some(transitionToStatus), messageReceived)
    }
}
