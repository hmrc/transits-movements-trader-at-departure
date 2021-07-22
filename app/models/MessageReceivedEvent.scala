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

import MessageReceivedEvent._

sealed trait MessageReceivedEvent {

  def targetStatus: DepartureStatus = this match {
    case CancellationDecision                 => DepartureStatus.CancellationDecision
    case ControlDecisionNotification          => DepartureStatus.ControlDecisionNotification
    case DeclarationCancellationRequest       => DepartureStatus.DeclarationCancellationRequest
    case DepartureRejected                    => DepartureStatus.DepartureRejected
    case DepartureSubmitted                   => DepartureStatus.DepartureSubmitted
    case GuaranteeNotValid                    => DepartureStatus.GuaranteeNotValid
    case MrnAllocated                         => DepartureStatus.MrnAllocated
    case NoReleaseForTransit                  => DepartureStatus.NoReleaseForTransit
    case PositiveAcknowledgement              => DepartureStatus.PositiveAcknowledgement
    case ReleaseForTransit                    => DepartureStatus.ReleaseForTransit
    case WriteOffNotification                 => DepartureStatus.WriteOffNotification
    case XMLSubmissionNegativeAcknowledgement => DepartureStatus.XMLSubmissionNegativeAcknowledgement
  }
}

object MessageReceivedEvent {

  case object DepartureSubmitted                   extends MessageReceivedEvent
  case object DepartureRejected                    extends MessageReceivedEvent
  case object MrnAllocated                         extends MessageReceivedEvent
  case object PositiveAcknowledgement              extends MessageReceivedEvent
  case object ControlDecisionNotification          extends MessageReceivedEvent
  case object NoReleaseForTransit                  extends MessageReceivedEvent
  case object ReleaseForTransit                    extends MessageReceivedEvent
  case object DeclarationCancellationRequest       extends MessageReceivedEvent
  case object CancellationDecision                 extends MessageReceivedEvent
  case object WriteOffNotification                 extends MessageReceivedEvent
  case object GuaranteeNotValid                    extends MessageReceivedEvent
  case object XMLSubmissionNegativeAcknowledgement extends MessageReceivedEvent

  val values: Seq[MessageReceivedEvent] = Seq(
    DepartureSubmitted,
    DepartureRejected,
    PositiveAcknowledgement,
    MrnAllocated,
    ControlDecisionNotification,
    NoReleaseForTransit,
    ReleaseForTransit,
    DeclarationCancellationRequest,
    CancellationDecision,
    WriteOffNotification,
    GuaranteeNotValid,
    XMLSubmissionNegativeAcknowledgement
  )
}
