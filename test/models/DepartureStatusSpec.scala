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

package models

import base.SpecBase
import generators.ModelGenerators
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DepartureStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ScalaCheckPropertyChecks with ModelGenerators {

  "DepartureStatus.values must contain" - {
    "Initialized" in DepartureStatus.values.contains(DepartureStatus.Initialized)
    "DepartureSubmitted" in DepartureStatus.values.contains(DepartureStatus.DepartureSubmitted)
    "MrnAllocated" in DepartureStatus.values.contains(DepartureStatus.MrnAllocated)
    "PositiveAcknowledgement" in DepartureStatus.values.contains(DepartureStatus.PositiveAcknowledgement)
    "DepartureRejected" in DepartureStatus.values.contains(DepartureStatus.DepartureRejected)
    "ControlDecisionNotification" in DepartureStatus.values.contains(DepartureStatus.ControlDecisionNotification)
    "ReleaseForTransit" in DepartureStatus.values.contains(DepartureStatus.ReleaseForTransit)
    "NoReleaseForTransit" in DepartureStatus.values.contains(DepartureStatus.NoReleaseForTransit)
    "DeclarationCancellationRequest" in DepartureStatus.values.contains(DepartureStatus.DeclarationCancellationRequest)
    "CancellationDecision" in DepartureStatus.values.contains(DepartureStatus.CancellationDecision)
    "WriteOffNotification" in DepartureStatus.values.contains(DepartureStatus.WriteOffNotification)
  }

}
