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

package audit

object AuditType {

  object User {
    val DepartureDeclarationSubmitted         = "DepartureDeclarationSubmitted"
    val DepartureCancellationRequestSubmitted = "DepartureCancellationRequestSubmitted"
  }

  object NCTS {
    val PositiveAcknowledgementReceived     = "PositiveAcknowledgementReceived"
    val MrnAllocatedReceived                = "MrnAllocatedReceived"
    val DeclarationRejectedReceived         = "DeclarationRejectedReceived"
    val ControlDecisionNotificationReceived = "ControlDecisionNotificationReceived"
    val NoReleaseForTransitReceived         = "NoReleaseForTransitReceived"
    val ReleaseForTransitReceived           = "ReleaseForTransitReceived"
    val CancellationDecisionReceived        = "CancellationDecisionReceived"
    val WriteOffNotificationReceived        = "WriteOffNotificationReceived"
    val GuaranteeNotValidReceived           = "GuaranteeNotValidReceived"
  }

}
