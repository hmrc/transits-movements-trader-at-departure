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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DepartureStatusSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ScalaCheckPropertyChecks with ModelGenerators {

  "Initialized must" - {

    "transition to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.DepartureSubmitted) mustEqual Right(DepartureStatus.DepartureSubmitted)
    }

    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.Initialized.transition(MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DepartureSubmitted, MessageReceivedEvent.MrnAllocated, MessageReceivedEvent.DepartureRejected)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.Initialized.transition(m).isLeft mustBe true
      }
    }
  }

  "DepartureSubmitted must" - {
    "transition to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.DepartureSubmitted) mustEqual Right(DepartureStatus.DepartureSubmitted)
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(DepartureStatus.PositiveAcknowledgement)
    }

    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.DepartureSubmitted.transition(MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
    }

    "return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.DepartureSubmitted,
        MessageReceivedEvent.PositiveAcknowledgement,
        MessageReceivedEvent.MrnAllocated,
        MessageReceivedEvent.DepartureRejected
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.DepartureSubmitted.transition(m).isLeft mustBe true
      }
    }
  }

  "PositiveAcknowledgement must " - {
    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      DepartureStatus.PositiveAcknowledgement.transition(MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      DepartureStatus.PositiveAcknowledgement.transition(MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(DepartureStatus.PositiveAcknowledgement)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.MrnAllocated, MessageReceivedEvent.PositiveAcknowledgement)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.PositiveAcknowledgement.transition(m).isLeft mustBe true
      }
    }
  }

  "MrnAllocated must " - {
    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.ControlDecisionNotification) mustEqual Right(DepartureStatus.ControlDecisionNotification)
    }

    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "transition to GuaranteeNotValid when receiving a GuaranteeNotValid event" in {
      DepartureStatus.MrnAllocated.transition(MessageReceivedEvent.GuaranteeNotValid) mustEqual Right(DepartureStatus.GuaranteeNotValid)
    }

    "return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.MrnAllocated,
        MessageReceivedEvent.ControlDecisionNotification,
        MessageReceivedEvent.NoReleaseForTransit,
        MessageReceivedEvent.ReleaseForTransit,
        MessageReceivedEvent.DeclarationCancellationRequest,
        MessageReceivedEvent.GuaranteeNotValid
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.MrnAllocated.transition(m).isLeft mustBe true
      }
    }
  }

  "DepartureRejected must " - {
    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      DepartureStatus.DepartureRejected.transition(MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DepartureRejected)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.DepartureRejected.transition(m).isLeft mustBe true
      }
    }
  }

  "ControlDecisionNotification must " - {
    "transition to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      DepartureStatus.ControlDecisionNotification.transition(MessageReceivedEvent.ControlDecisionNotification) mustEqual Right(
        DepartureStatus.ControlDecisionNotification)
    }

    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      DepartureStatus.ControlDecisionNotification.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      DepartureStatus.ControlDecisionNotification.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "return an error message when receiving any other event" in {
      val validMessages =
        Seq(MessageReceivedEvent.ControlDecisionNotification, MessageReceivedEvent.NoReleaseForTransit, MessageReceivedEvent.ReleaseForTransit)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.ControlDecisionNotification.transition(m).isLeft mustBe true
      }
    }
  }

  "ReleaseForTransit must " - {
    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.CancellationDecision) mustEqual Right(DepartureStatus.CancellationDecision)
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      DepartureStatus.ReleaseForTransit.transition(MessageReceivedEvent.WriteOffNotification) mustEqual Right(DepartureStatus.WriteOffNotification)
    }

    "return an error message when receiving any other event" in {
      val validMessages =
        Seq(
          MessageReceivedEvent.ReleaseForTransit,
          MessageReceivedEvent.DeclarationCancellationRequest,
          MessageReceivedEvent.CancellationDecision,
          MessageReceivedEvent.WriteOffNotification
        )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.ReleaseForTransit.transition(m).isLeft mustBe true
      }
    }
  }

  "NoReleaseForTransit must " - {
    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      DepartureStatus.NoReleaseForTransit.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(DepartureStatus.NoReleaseForTransit)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.NoReleaseForTransit)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.NoReleaseForTransit.transition(m).isLeft mustBe true
      }
    }
  }

  "DeclarationCancellationRequest must " - {
    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      DepartureStatus.DeclarationCancellationRequest.transition(MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      DepartureStatus.DeclarationCancellationRequest.transition(MessageReceivedEvent.CancellationDecision) mustEqual Right(DepartureStatus.CancellationDecision)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DeclarationCancellationRequest, MessageReceivedEvent.CancellationDecision)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.DeclarationCancellationRequest.transition(m).isLeft mustBe true
      }
    }
  }

  "CancellationDecision must " - {
    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      DepartureStatus.CancellationDecision.transition(MessageReceivedEvent.CancellationDecision) mustEqual Right(DepartureStatus.CancellationDecision)
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      DepartureStatus.CancellationDecision.transition(MessageReceivedEvent.WriteOffNotification) mustEqual Right(DepartureStatus.WriteOffNotification)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.CancellationDecision, MessageReceivedEvent.WriteOffNotification)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.CancellationDecision.transition(m).isLeft mustBe true
      }
    }
  }

  "WriteOffNotification must " - {
    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      DepartureStatus.WriteOffNotification.transition(MessageReceivedEvent.WriteOffNotification) mustEqual Right(DepartureStatus.WriteOffNotification)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.WriteOffNotification)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.WriteOffNotification.transition(m).isLeft mustBe true
      }
    }
  }

  "GuaranteeNotValid must " - {
    "transition to GuaranteeNotValid when recieving a GuaranteeNotValid event" in {
      DepartureStatus.GuaranteeNotValid.transition(MessageReceivedEvent.GuaranteeNotValid) mustEqual Right(DepartureStatus.GuaranteeNotValid)
    }

    "transition to NoReleaseForTransit when recieving a NoReleaseForTransit event" in {
      DepartureStatus.GuaranteeNotValid.transition(MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when recieving a ReleaseForTransit event" in {
      DepartureStatus.GuaranteeNotValid.transition(MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.GuaranteeNotValid, MessageReceivedEvent.NoReleaseForTransit, MessageReceivedEvent.ReleaseForTransit)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          DepartureStatus.GuaranteeNotValid.transition(m).isLeft mustBe true
      }
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
    "WriteOffNotification" in { DepartureStatus.values.contains(DepartureStatus.WriteOffNotification) }
  }

}
