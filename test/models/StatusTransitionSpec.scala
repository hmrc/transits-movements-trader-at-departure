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

import base.SpecBase
import generators.ModelGenerators
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StatusTransitionSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ScalaCheckPropertyChecks with ModelGenerators {

  "Initialized must" - {

    "transition to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      StatusTransition.transition(DepartureStatus.Initialized, MessageReceivedEvent.DepartureSubmitted) mustEqual Right(DepartureStatus.DepartureSubmitted)
    }

    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.transition(DepartureStatus.Initialized, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.transition(DepartureStatus.Initialized, MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DepartureSubmitted, MessageReceivedEvent.MrnAllocated, MessageReceivedEvent.DepartureRejected)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.Initialized, m).isLeft mustBe true
      }
    }
  }

  "DepartureSubmitted must" - {
    "transition to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      StatusTransition.transition(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.DepartureSubmitted) mustEqual Right(
        DepartureStatus.DepartureSubmitted)
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      StatusTransition.transition(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(
        DepartureStatus.PositiveAcknowledgement)
    }

    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.transition(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.transition(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
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
          StatusTransition.transition(DepartureStatus.DepartureSubmitted, m).isLeft mustBe true
      }
    }
  }

  "PositiveAcknowledgement must " - {
    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.transition(DepartureStatus.PositiveAcknowledgement, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      StatusTransition.transition(DepartureStatus.PositiveAcknowledgement, MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(
        DepartureStatus.PositiveAcknowledgement)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.MrnAllocated, MessageReceivedEvent.PositiveAcknowledgement)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.PositiveAcknowledgement, m).isLeft mustBe true
      }
    }
  }

  "MrnAllocated must " - {
    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.transition(DepartureStatus.MrnAllocated, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      StatusTransition.transition(DepartureStatus.MrnAllocated, MessageReceivedEvent.ControlDecisionNotification) mustEqual Right(
        DepartureStatus.ControlDecisionNotification)
    }

    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.MrnAllocated, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.MrnAllocated, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.transition(DepartureStatus.MrnAllocated, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "transition to GuaranteeNotValid when receiving a GuaranteeNotValid event" in {
      StatusTransition.transition(DepartureStatus.MrnAllocated, MessageReceivedEvent.GuaranteeNotValid) mustEqual Right(DepartureStatus.GuaranteeNotValid)
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
          StatusTransition.transition(DepartureStatus.MrnAllocated, m).isLeft mustBe true
      }
    }
  }

  "DepartureRejected must " - {
    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.transition(DepartureStatus.DepartureRejected, MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DepartureRejected)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.DepartureRejected, m).isLeft mustBe true
      }
    }
  }

  "ControlDecisionNotification must " - {
    "transition to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      StatusTransition.transition(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.ControlDecisionNotification) mustEqual Right(
        DepartureStatus.ControlDecisionNotification)
    }

    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(
        DepartureStatus.ReleaseForTransit)
    }

    "return an error message when receiving any other event" in {
      val validMessages =
        Seq(MessageReceivedEvent.ControlDecisionNotification, MessageReceivedEvent.NoReleaseForTransit, MessageReceivedEvent.ReleaseForTransit)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.ControlDecisionNotification, m).isLeft mustBe true
      }
    }
  }

  "ReleaseForTransit must " - {
    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.transition(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      StatusTransition.transition(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.CancellationDecision) mustEqual Right(
        DepartureStatus.CancellationDecision)
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.transition(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification)
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
          StatusTransition.transition(DepartureStatus.ReleaseForTransit, m).isLeft mustBe true
      }
    }
  }

  "NoReleaseForTransit must " - {
    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.NoReleaseForTransit, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.NoReleaseForTransit)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.NoReleaseForTransit, m).isLeft mustBe true
      }
    }
  }

  "DeclarationCancellationRequest must " - {
    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.transition(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      StatusTransition.transition(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.CancellationDecision) mustEqual Right(
        DepartureStatus.CancellationDecision)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DeclarationCancellationRequest, MessageReceivedEvent.CancellationDecision)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.DeclarationCancellationRequest, m).isLeft mustBe true
      }
    }
  }

  "CancellationDecision must " - {
    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      StatusTransition.transition(DepartureStatus.CancellationDecision, MessageReceivedEvent.CancellationDecision) mustEqual Right(
        DepartureStatus.CancellationDecision)
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.transition(DepartureStatus.CancellationDecision, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.CancellationDecision, MessageReceivedEvent.WriteOffNotification)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.CancellationDecision, m).isLeft mustBe true
      }
    }
  }

  "WriteOffNotification must " - {
    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.transition(DepartureStatus.WriteOffNotification, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification)
    }

    "return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.WriteOffNotification)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.WriteOffNotification, m).isLeft mustBe true
      }
    }
  }

  "GuaranteeNotValid must " - {
    "transition to GuaranteeNotValid when recieving a GuaranteeNotValid event" in {
      StatusTransition.transition(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.GuaranteeNotValid) mustEqual Right(DepartureStatus.GuaranteeNotValid)
    }

    "transition to NoReleaseForTransit when recieving a NoReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when recieving a ReleaseForTransit event" in {
      StatusTransition.transition(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when recieving a DeclarationCancellationRequest event" in {
      StatusTransition.transition(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest)
    }

    "return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.GuaranteeNotValid,
        MessageReceivedEvent.NoReleaseForTransit,
        MessageReceivedEvent.ReleaseForTransit,
        MessageReceivedEvent.DeclarationCancellationRequest
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          StatusTransition.transition(DepartureStatus.GuaranteeNotValid, m).isLeft mustBe true
      }
    }
  }

}
