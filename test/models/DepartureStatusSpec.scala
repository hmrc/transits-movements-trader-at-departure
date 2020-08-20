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

import base.SpecBase
import generators.ModelGenerators
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DepartureStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "Initialized must transition" - {

    "to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.DepartureSubmitted) mustEqual DepartureStatus.DepartureSubmitted
    }

    "to MrnAllocated when receiving a MrnAllocated event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.MrnAllocated) mustEqual DepartureStatus.MrnAllocated
    }

    "to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.DepartureRejected) mustEqual DepartureStatus.DepartureRejected
    }
  }

  "DepartureSubmitted must transition" - {
    "to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.DepartureSubmitted) mustEqual DepartureStatus.DepartureSubmitted
    }

    "to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.PositiveAcknowledgement) mustEqual DepartureStatus.PositiveAcknowledgement
    }

    "to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.DepartureRejected) mustEqual DepartureStatus.DepartureRejected
    }
  }

  "PositiveAcknowledement must transition" - {
    "to MrnAllocated when recieving a MrnAllocated event" in {
      DepartureStatus.PositiveAcknowledgement.transition(MessageReceivedEvent.MrnAllocated) mustEqual DepartureStatus.MrnAllocated
    }
  }

  "MrnAllocated must transition" - {
    "to MrnAllocated when receiving a MrnAllocated event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.MrnAllocated) mustEqual DepartureStatus.MrnAllocated
    }

    "to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.ControlDecisionNotification) mustEqual (DepartureStatus.ControlDecisionNotification)
    }

    "to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual DepartureStatus.NoReleaseForTransit
    }

    "to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual DepartureStatus.ReleaseForTransit
    }

    "to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.DeclarationCancellationRequest) mustEqual DepartureStatus.DeclarationCancellationRequest
    }
  }

  "DepartureRejected must transition" - {
    "to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.DepartureRejected.transition(MessageReceivedEvent.DepartureRejected) mustEqual DepartureStatus.DepartureRejected
    }
  }

  "ControlDecisionNotification must transition" - {
    "to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      DepartureStatus.ControlDecisionNotification.transition(MessageReceivedEvent.ControlDecisionNotification) mustEqual DepartureStatus.ControlDecisionNotification
    }

    "to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      DepartureStatus.ControlDecisionNotification.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual DepartureStatus.NoReleaseForTransit
    }

    "to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      DepartureStatus.ControlDecisionNotification.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual DepartureStatus.ReleaseForTransit
    }
  }

  "ReleaseForTransit must transition" - {
    "to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual DepartureStatus.ReleaseForTransit
    }

    "to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.DeclarationCancellationRequest) mustEqual DepartureStatus.DeclarationCancellationRequest
    }

    "to CancellationDecision when receiving a CancellationDecision event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.CancellationDecision) mustEqual DepartureStatus.CancellationDecision
    }
  }

  "NoReleaseForTransit must transition" - {
    "to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      DepartureStatus.NoReleaseForTransit.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual DepartureStatus.NoReleaseForTransit
    }
  }

  "DeclarationCancellationRequest must transition" - {
    "to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      DepartureStatus.DeclarationCancellationRequest.transition(MessageReceivedEvent.DeclarationCancellationRequest) mustEqual DepartureStatus.DeclarationCancellationRequest
    }

    "to CancellationDecision when receiving a CancellationDecision event" in {
      DepartureStatus.DeclarationCancellationRequest.transition(MessageReceivedEvent.CancellationDecision) mustEqual DepartureStatus.CancellationDecision
    }
  }

  "DepartureStatus.values must contain" - {
    "Initialized" in { DepartureStatus.values.contains(DepartureStatus.Initialized) }
    "DepartureSubmitted" in { DepartureStatus.values.contains(DepartureStatus.DepartureSubmitted) }
    "MrnAllocated" in { DepartureStatus.values.contains(DepartureStatus.MrnAllocated) }
    "PositiveAcknowledgement" in { DepartureStatus.values.contains(DepartureStatus.PositiveAcknowledgement) }
    "DepartureRejected" in { DepartureStatus.values.contains(DepartureStatus.DepartureRejected) }
    "ControlDecisionNotification" in { DepartureStatus.values.contains(DepartureStatus.ControlDecisionNotification) }
    "ReleaseForTransit" in { DepartureStatus.values.contains(DepartureStatus.ReleaseForTransit) }
    "NoReleaseForTransit" in { DepartureStatus.values.contains(DepartureStatus.NoReleaseForTransit) }
    "DeclarationCancellationRequest" in { DepartureStatus.values.contains(DepartureStatus.DeclarationCancellationRequest) }
    "CancellationDecision" in { DepartureStatus.values.contains(DepartureStatus.CancellationDecision) }
  }

}
