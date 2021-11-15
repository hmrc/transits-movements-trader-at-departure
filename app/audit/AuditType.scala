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

package audit

import models.Enumerable

sealed trait AuditType {}

object AuditType extends Enumerable.Implicits {

  case object SuccessfulAuthTracking extends AuditType

  // User
  case object DepartureDeclarationSubmitted         extends AuditType
  case object DepartureCancellationRequestSubmitted extends AuditType

  //Transform
  case object MesSenMES3Added extends AuditType

  // NCTS
  case object PositiveAcknowledgementReceived      extends AuditType
  case object MrnAllocatedReceived                 extends AuditType
  case object DeclarationRejectedReceived          extends AuditType
  case object ControlDecisionNotificationReceived  extends AuditType
  case object NoReleaseForTransitReceived          extends AuditType
  case object ReleaseForTransitReceived            extends AuditType
  case object CancellationDecisionReceived         extends AuditType
  case object WriteOffNotificationReceived         extends AuditType
  case object GuaranteeNotValidReceived            extends AuditType
  case object XMLSubmissionNegativeAcknowledgement extends AuditType

  val values: Seq[AuditType] =
    Seq(
      DepartureDeclarationSubmitted,
      DepartureCancellationRequestSubmitted,
      PositiveAcknowledgementReceived,
      MrnAllocatedReceived,
      DeclarationRejectedReceived,
      ControlDecisionNotificationReceived,
      NoReleaseForTransitReceived,
      ReleaseForTransitReceived,
      CancellationDecisionReceived,
      WriteOffNotificationReceived,
      GuaranteeNotValidReceived,
      MesSenMES3Added,
      XMLSubmissionNegativeAcknowledgement
    )
}
