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

import models.XSDFile._

sealed trait MessageResponse {
  val messageReceived: MessageReceivedEvent
  val messageType: MessageType
  val xsdFile: XSDFile
}

case object MrnAllocatedResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.MrnAllocated
  override val messageType: MessageType              = MessageType.MrnAllocated
  override val xsdFile: XSDFile                      = MRNAllocatedXSD
}

case object DepartureRejectedResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.DepartureRejected
  override val messageType: MessageType              = MessageType.DeclarationRejected
  override val xsdFile: XSDFile                      = DepartureRejectedXSD
}

case object PositiveAcknowledgementResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.PositiveAcknowledgement
  override val messageType: MessageType              = MessageType.PositiveAcknowledgement
  override val xsdFile: XSDFile                      = PositiveAcknowledgementXSD
}

case object ControlDecisionNotificationResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.ControlDecisionNotification
  override val messageType: MessageType              = MessageType.ControlDecisionNotification
  override val xsdFile: XSDFile                      = ControlDecisionNotificationXSD
}

case object NoReleaseForTransitResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.NoReleaseForTransit
  override val messageType: MessageType              = MessageType.NoReleaseForTransit
  override val xsdFile: XSDFile                      = NoReleaseForTransitXSD
}

case object ReleaseForTransitResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.ReleaseForTransit
  override val messageType: MessageType              = MessageType.ReleaseForTransit
  override val xsdFile: XSDFile                      = ReleaseForTransitXSD
}

case object CancellationDecisionResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.CancellationDecision
  override val messageType: MessageType              = MessageType.CancellationDecision
  override val xsdFile: XSDFile                      = CancellationDecisionXSD
}

case object WriteOffNotificationResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.WriteOffNotification
  override val messageType: MessageType              = MessageType.WriteOffNotification
  override val xsdFile: XSDFile                      = WriteOffNotificationXSD
}

case object GuaranteeNotValidResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.GuaranteeNotValid
  override val messageType: MessageType              = MessageType.GuaranteeNotValid
  override val xsdFile: XSDFile                      = GuaranteeNotValidXSD
}
