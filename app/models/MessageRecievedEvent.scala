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

package models

sealed trait MessageReceivedEvent

object MessageReceivedEvent {

  case object DepartureSubmitted             extends MessageReceivedEvent
  case object DepartureRejected              extends MessageReceivedEvent
  case object MrnAllocated                   extends MessageReceivedEvent
  case object PositiveAcknowledgement        extends MessageReceivedEvent
  case object ControlDecisionNotification    extends MessageReceivedEvent
  case object NoReleaseForTransit            extends MessageReceivedEvent
  case object ReleaseForTransit              extends MessageReceivedEvent
  case object DeclarationCancellationRequest extends MessageReceivedEvent

  val values: Seq[MessageReceivedEvent] = Seq(
    DepartureSubmitted,
    DepartureRejected,
    PositiveAcknowledgement,
    MrnAllocated,
    ControlDecisionNotification,
    NoReleaseForTransit,
    ReleaseForTransit,
    DeclarationCancellationRequest
  )
}
