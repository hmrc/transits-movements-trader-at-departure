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
    // DepartureSubmitted transitions
    DepartureSubmitted -> DepartureSubmitted,
    DepartureSubmitted -> PositiveAcknowledgement,
    DepartureSubmitted -> MrnAllocated,
    DepartureSubmitted -> DepartureRejected,
    // PositiveAcknowledgement transitions
    PositiveAcknowledgement -> PositiveAcknowledgement,
    PositiveAcknowledgement -> MrnAllocated,
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
    // NoReleaseForTransit transitions
    NoReleaseForTransit -> NoReleaseForTransit,
    // ReleaseForTransit transitions
    ReleaseForTransit -> ReleaseForTransit,
    ReleaseForTransit -> DeclarationCancellationRequest,
    ReleaseForTransit -> CancellationDecision,
    ReleaseForTransit -> WriteOffNotification,
    // DeclarationCancellationRequest transitions
    DeclarationCancellationRequest -> DeclarationCancellationRequest,
    DeclarationCancellationRequest -> CancellationDecision,
    // CancellationDecision transitions
    CancellationDecision -> CancellationDecision,
    CancellationDecision -> WriteOffNotification,
    // WriteOffNotification transitions
    WriteOffNotification -> WriteOffNotification,
    // GuaranteeNotValid transitions
    GuaranteeNotValid -> GuaranteeNotValid,
    GuaranteeNotValid -> NoReleaseForTransit,
    GuaranteeNotValid -> ReleaseForTransit,
    GuaranteeNotValid -> DeclarationCancellationRequest
  )

  /** Mapping of the statuses we can transition to from a given status
    */
  val allowedStatusForTransition = validTransitions
    .groupBy {
      case (from, to) => from
    }
    .mapValues(_.map {
      case (from, to) => to
    })
    .toMap

  /** Mapping of the statuses that are able to transition to a given status
    */
  val requiredStatusForTransition = validTransitions
    .groupBy {
      case (from, to) => to
    }
    .mapValues(_.map {
      case (from, to) => from
    })
    .toMap

  def transitionError(
    currentStatus: DepartureStatus,
    requiredStatuses: Set[DepartureStatus],
    targetStatus: DepartureStatus,
    messageReceived: MessageReceivedEvent
  ): Either[TransitionError, DepartureStatus] = {
    val messageType = messageReceived.toString

    val requiredStatusesString = requiredStatuses
      .filterNot(_ == targetStatus)
      .map(_.toString)
      .toList
      .sorted
      .mkString(" or ")

    Left(
      TransitionError(
        s"Can only accept a message of type [$messageType] directly after [$requiredStatusesString] messages. Current state is [${currentStatus.toString}]."
      )
    )
  }

  def targetStatus(messageReceived: MessageReceivedEvent) = messageReceived match {
    case MessageReceivedEvent.CancellationDecision           => CancellationDecision
    case MessageReceivedEvent.ControlDecisionNotification    => ControlDecisionNotification
    case MessageReceivedEvent.DeclarationCancellationRequest => DeclarationCancellationRequest
    case MessageReceivedEvent.DepartureRejected              => DepartureRejected
    case MessageReceivedEvent.DepartureSubmitted             => DepartureSubmitted
    case MessageReceivedEvent.GuaranteeNotValid              => GuaranteeNotValid
    case MessageReceivedEvent.MrnAllocated                   => MrnAllocated
    case MessageReceivedEvent.NoReleaseForTransit            => NoReleaseForTransit
    case MessageReceivedEvent.PositiveAcknowledgement        => PositiveAcknowledgement
    case MessageReceivedEvent.ReleaseForTransit              => ReleaseForTransit
    case MessageReceivedEvent.WriteOffNotification           => WriteOffNotification
  }

  def transition(currentStatus: DepartureStatus, messageReceived: MessageReceivedEvent): Either[TransitionError, DepartureStatus] = {
    val transitionToStatus      = targetStatus(messageReceived)
    val allowedFromThisStatus   = allowedStatusForTransition.get(currentStatus).getOrElse(Set.empty)
    val requiredForTargetStatus = requiredStatusForTransition.get(transitionToStatus).getOrElse(Set.empty)

    if (allowedFromThisStatus.contains(transitionToStatus))
      Right(transitionToStatus)
    else
      transitionError(currentStatus, requiredForTargetStatus, transitionToStatus, messageReceived)
  }
}
