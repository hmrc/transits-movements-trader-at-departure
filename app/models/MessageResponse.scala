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

import models.XSDFile.DepartureRejectedXSD
import models.XSDFile.MRNAllocatedXSD

sealed trait MessageResponse {
  val messageReceived: MessageReceivedEvent
  val messageType: MessageType
  val xsdFile: XSDFile
}

case object MRNAllocatedResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.MRNAllocated
  override val messageType: MessageType              = MessageType.MRNAllocated
  override val xsdFile: XSDFile                      = MRNAllocatedXSD
}

case object DepartureRejectedResponse extends MessageResponse {
  override val messageReceived: MessageReceivedEvent = MessageReceivedEvent.DepartureRejected
  override val messageType: MessageType              = MessageType.DeclarationRejected
  override val xsdFile: XSDFile                      = DepartureRejectedXSD
}
