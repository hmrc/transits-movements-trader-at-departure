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
      StatusTransition.targetStatus(DepartureStatus.Initialized, MessageReceivedEvent.DepartureSubmitted) mustEqual Right(DepartureStatus.DepartureSubmitted)
    }

    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.targetStatus(DepartureStatus.Initialized, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.targetStatus(DepartureStatus.Initialized, MessageReceivedEvent.DepartureRejected) mustEqual Right(DepartureStatus.DepartureRejected)
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      StatusTransition.targetStatus(DepartureStatus.Initialized, MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(
        DepartureStatus.PositiveAcknowledgement
      )
    }

    "transition to DepartureSubmittedNegativeAcknowledgement when receiving an XMLSubmissionNegativeAcknowledgement event" in {
      StatusTransition.targetStatus(DepartureStatus.Initialized, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement) mustEqual Right(
        DepartureStatus.DepartureSubmittedNegativeAcknowledgement
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.DepartureSubmitted,
        MessageReceivedEvent.MrnAllocated,
        MessageReceivedEvent.DepartureRejected,
        MessageReceivedEvent.PositiveAcknowledgement,
        MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.Initialized}:") {
            StatusTransition.targetStatus(DepartureStatus.Initialized, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "DepartureSubmitted must" - {
    "transition to DepartureSubmitted when receiving a DepartureSubmitted event" in {
      StatusTransition.targetStatus(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.DepartureSubmitted) mustEqual Right(
        DepartureStatus.DepartureSubmitted
      )
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      StatusTransition.targetStatus(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(
        DepartureStatus.PositiveAcknowledgement
      )
    }

    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.targetStatus(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.targetStatus(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.DepartureRejected) mustEqual Right(
        DepartureStatus.DepartureRejected)
    }

    "transition to DepartureSubmittedNegativeAcknowledgement when receiving an XMLSubmissionNegativeAcknowledgement event" in {
      StatusTransition.targetStatus(DepartureStatus.DepartureSubmitted, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement) mustEqual Right(
        DepartureStatus.DepartureSubmittedNegativeAcknowledgement
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.DepartureSubmitted,
        MessageReceivedEvent.PositiveAcknowledgement,
        MessageReceivedEvent.MrnAllocated,
        MessageReceivedEvent.DepartureRejected,
        MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.DepartureSubmitted}:") {
            StatusTransition.targetStatus(DepartureStatus.DepartureSubmitted, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "PositiveAcknowledgement must " - {
    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.targetStatus(DepartureStatus.PositiveAcknowledgement, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to PositiveAcknowledgement when receiving a PositiveAcknowledgement event" in {
      StatusTransition.targetStatus(DepartureStatus.PositiveAcknowledgement, MessageReceivedEvent.PositiveAcknowledgement) mustEqual Right(
        DepartureStatus.PositiveAcknowledgement
      )
    }

    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.targetStatus(DepartureStatus.PositiveAcknowledgement, MessageReceivedEvent.DepartureRejected) mustEqual Right(
        DepartureStatus.DepartureRejected
      )
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.targetStatus(DepartureStatus.PositiveAcknowledgement, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.MrnAllocated,
        MessageReceivedEvent.PositiveAcknowledgement,
        MessageReceivedEvent.DepartureRejected,
        MessageReceivedEvent.WriteOffNotification
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.PositiveAcknowledgement}:") {
            StatusTransition.targetStatus(DepartureStatus.PositiveAcknowledgement, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "MrnAllocated must " - {
    "transition to MrnAllocated when receiving a MrnAllocated event" in {
      StatusTransition.targetStatus(DepartureStatus.MrnAllocated, MessageReceivedEvent.MrnAllocated) mustEqual Right(DepartureStatus.MrnAllocated)
    }

    "transition to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      StatusTransition.targetStatus(DepartureStatus.MrnAllocated, MessageReceivedEvent.ControlDecisionNotification) mustEqual Right(
        DepartureStatus.ControlDecisionNotification
      )
    }

    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.MrnAllocated, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(DepartureStatus.NoReleaseForTransit)
    }

    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.MrnAllocated, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.targetStatus(DepartureStatus.MrnAllocated, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest
      )
    }

    "transition to GuaranteeNotValid when receiving a GuaranteeNotValid event" in {
      StatusTransition.targetStatus(DepartureStatus.MrnAllocated, MessageReceivedEvent.GuaranteeNotValid) mustEqual Right(DepartureStatus.GuaranteeNotValid)
    }

    "don't return an error message when receiving any other event" in {
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
          withClue(s"Received event $m at status ${DepartureStatus.MrnAllocated}:") {
            StatusTransition.targetStatus(DepartureStatus.MrnAllocated, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "DepartureRejected must " - {
    "transition to DepartureRejected when receiving a DepartureRejected event" in {
      StatusTransition.targetStatus(DepartureStatus.DepartureRejected, MessageReceivedEvent.DepartureRejected) mustEqual Right(
        DepartureStatus.DepartureRejected)
    }

    "don't return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.DepartureRejected)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.DepartureRejected}:") {
            StatusTransition.targetStatus(DepartureStatus.DepartureRejected, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "ControlDecisionNotification must " - {
    "transition to ControlDecisionNotification when receiving a ControlDecisionNotification event" in {
      StatusTransition.targetStatus(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.ControlDecisionNotification) mustEqual Right(
        DepartureStatus.ControlDecisionNotification
      )
    }

    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit
      )
    }

    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(
        DepartureStatus.ReleaseForTransit
      )
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.targetStatus(DepartureStatus.ControlDecisionNotification, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages =
        Seq(
          MessageReceivedEvent.ControlDecisionNotification,
          MessageReceivedEvent.NoReleaseForTransit,
          MessageReceivedEvent.ReleaseForTransit,
          MessageReceivedEvent.DeclarationCancellationRequest
        )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.ControlDecisionNotification}:") {
            StatusTransition.targetStatus(DepartureStatus.ControlDecisionNotification, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "ReleaseForTransit must " - {
    "transition to ReleaseForTransit when receiving a ReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(
        DepartureStatus.ReleaseForTransit)
    }

    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      StatusTransition.targetStatus(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.CancellationDecision) mustEqual Right(
        DepartureStatus.CancellationDecision
      )
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.targetStatus(DepartureStatus.ReleaseForTransit, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages =
        Seq(
          MessageReceivedEvent.ReleaseForTransit,
          MessageReceivedEvent.CancellationDecision,
          MessageReceivedEvent.WriteOffNotification
        )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.ReleaseForTransit}:") {
            StatusTransition.targetStatus(DepartureStatus.ReleaseForTransit, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "NoReleaseForTransit must " - {
    "transition to NoReleaseForTransit when receiving a NoReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.NoReleaseForTransit, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.NoReleaseForTransit)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.NoReleaseForTransit}:") {
            StatusTransition.targetStatus(DepartureStatus.NoReleaseForTransit, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "DeclarationCancellationRequest must " - {
    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest
      )
    }

    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.CancellationDecision) mustEqual Right(
        DepartureStatus.CancellationDecision
      )
    }

    "transition to DeclarationCancellationRequestNegativeAcknowledgement when receiving an XMLSubmissionNegativeAcknowledgement event" in {
      StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
      )
    }

    "transition to ReleaseForTransit when receiving an ReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(
        DepartureStatus.ReleaseForTransit
      )
    }

    "transition to NoReleaseForTransit when receiving an NoReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequest, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.DeclarationCancellationRequest,
        MessageReceivedEvent.CancellationDecision,
        MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement,
        MessageReceivedEvent.ReleaseForTransit,
        MessageReceivedEvent.NoReleaseForTransit
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.DeclarationCancellationRequest}:") {
            StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequest, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "CancellationDecision must " - {
    "transition to CancellationDecision when receiving a CancellationDecision event" in {
      StatusTransition.targetStatus(DepartureStatus.CancellationDecision, MessageReceivedEvent.CancellationDecision) mustEqual Right(
        DepartureStatus.CancellationDecision
      )
    }

    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.targetStatus(DepartureStatus.CancellationDecision, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.CancellationDecision, MessageReceivedEvent.WriteOffNotification)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.CancellationDecision}:") {
            StatusTransition.targetStatus(DepartureStatus.CancellationDecision, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "WriteOffNotification must " - {
    "transition to WriteOffNotification when receiving a WriteOffNotification event" in {
      StatusTransition.targetStatus(DepartureStatus.WriteOffNotification, MessageReceivedEvent.WriteOffNotification) mustEqual Right(
        DepartureStatus.WriteOffNotification
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.WriteOffNotification)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.WriteOffNotification}:") {
            StatusTransition.targetStatus(DepartureStatus.WriteOffNotification, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "GuaranteeNotValid must " - {
    "transition to GuaranteeNotValid when recieving a GuaranteeNotValid event" in {
      StatusTransition.targetStatus(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.GuaranteeNotValid) mustEqual Right(
        DepartureStatus.GuaranteeNotValid)
    }

    "transition to NoReleaseForTransit when recieving a NoReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.NoReleaseForTransit) mustEqual Right(
        DepartureStatus.NoReleaseForTransit
      )
    }

    "transition to ReleaseForTransit when recieving a ReleaseForTransit event" in {
      StatusTransition.targetStatus(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.ReleaseForTransit) mustEqual Right(
        DepartureStatus.ReleaseForTransit)
    }

    "transition to DeclarationCancellationRequest when recieving a DeclarationCancellationRequest event" in {
      StatusTransition.targetStatus(DepartureStatus.GuaranteeNotValid, MessageReceivedEvent.DeclarationCancellationRequest) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages = Seq(
        MessageReceivedEvent.GuaranteeNotValid,
        MessageReceivedEvent.NoReleaseForTransit,
        MessageReceivedEvent.ReleaseForTransit,
        MessageReceivedEvent.DeclarationCancellationRequest
      )
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.GuaranteeNotValid}:") {
            StatusTransition.targetStatus(DepartureStatus.GuaranteeNotValid, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "DepartureSubmittedNegativeAcknowledgement must " - {
    "transition to DepartureSubmittedNegativeAcknowledgement when receiving an XMLSubmissionNegativeAcknowledgement event" in {
      StatusTransition.targetStatus(
        DepartureStatus.DepartureSubmittedNegativeAcknowledgement,
        MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
      ) mustEqual Right(
        DepartureStatus.DepartureSubmittedNegativeAcknowledgement
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.DepartureSubmittedNegativeAcknowledgement}:") {
            StatusTransition.targetStatus(DepartureStatus.DepartureSubmittedNegativeAcknowledgement, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

  "DeclarationCancellationRequestNegativeAcknowledgement must" - {
    "transition to DeclarationCancellationRequestNegativeAcknowledgement when receiving an XMLSubmissionNegativeAcknowledgement event" in {
      StatusTransition.targetStatus(
        DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement,
        MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement
      ) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
      )
    }

    "transition to DeclarationCancellationRequest when receiving a DeclarationCancellationRequest event" in {
      StatusTransition.targetStatus(
        DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement,
        MessageReceivedEvent.DeclarationCancellationRequest
      ) mustEqual Right(
        DepartureStatus.DeclarationCancellationRequest
      )
    }

    "don't return an error message when receiving any other event" in {
      val validMessages   = Seq(MessageReceivedEvent.XMLSubmissionNegativeAcknowledgement, MessageReceivedEvent.DeclarationCancellationRequest)
      val invalidMessages = MessageReceivedEvent.values.diff(validMessages)
      invalidMessages.foreach {
        m =>
          withClue(s"Received event $m at status ${DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement}:") {
            StatusTransition.targetStatus(DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement, m) mustBe a[Right[_, _]]
          }
      }
    }
  }

}
