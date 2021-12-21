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
import models.MessageType._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json._

class MessageTypeSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "MessageType.values must contain" - {
    "PositiveAcknowledgement" in {
      MessageType.values must contain(PositiveAcknowledgement)
      PositiveAcknowledgement.code mustEqual "IE928"
      PositiveAcknowledgement.rootNode mustEqual "CC928A"
    }

    "DepartureDeclaration" in {
      MessageType.values must contain(DepartureDeclaration)
      DepartureDeclaration.code mustEqual "IE015"
      DepartureDeclaration.rootNode mustEqual "CC015B"
    }

    "MrnAllocated" in {
      MessageType.values must contain(MrnAllocated)
      MrnAllocated.code mustEqual "IE028"
      MrnAllocated.rootNode mustEqual "CC028A"
    }

    "DeclarationRejected" in {
      MessageType.values must contain(DeclarationRejected)
      DeclarationRejected.code mustEqual "IE016"
      DeclarationRejected.rootNode mustEqual "CC016A"
    }

    "ControlDecisionNotification" in {
      MessageType.values must contain(ControlDecisionNotification)
      ControlDecisionNotification.code mustEqual "IE060"
      ControlDecisionNotification.rootNode mustEqual "CC060A"
    }

    "NoReleaseForTransit" in {
      MessageType.values must contain(NoReleaseForTransit)
      NoReleaseForTransit.code mustEqual "IE051"
      NoReleaseForTransit.rootNode mustEqual "CC051B"
    }

    "ReleaseForTransit" in {
      MessageType.values must contain(ReleaseForTransit)
      ReleaseForTransit.code mustEqual "IE029"
      ReleaseForTransit.rootNode mustEqual "CC029B"
    }

    "DeclarationCancellationRequest" in {
      MessageType.values must contain(DeclarationCancellationRequest)
      DeclarationCancellationRequest.code mustEqual "IE014"
      DeclarationCancellationRequest.rootNode mustEqual "CC014A"
    }

    "CancellationDecision" in {
      MessageType.values must contain(CancellationDecision)
      CancellationDecision.code mustEqual "IE009"
      CancellationDecision.rootNode mustEqual "CC009A"
    }

    "WriteOffNotification" in {
      MessageType.values must contain(WriteOffNotification)
      WriteOffNotification.code mustEqual "IE045"
      WriteOffNotification.rootNode mustEqual "CC045A"
    }

    "GuaranteeNotValid" in {
      MessageType.values must contain(GuaranteeNotValid)
      GuaranteeNotValid.code mustEqual "IE055"
      GuaranteeNotValid.rootNode mustEqual "CC055A"
    }
  }

  "Json reads and writes" - {
    "writes" in {
      forAll(arbitrary[MessageType]) {
        messageType =>
          Json.toJson(messageType) mustEqual JsString(messageType.code)
      }
    }

    "reads" - {
      "returns the message type when given the code for a message" in {
        forAll(arbitrary[MessageType]) {
          message =>
            JsString(message.code).validate[MessageType] mustEqual JsSuccess(message)
        }
      }

      "returns an error when the message type code is not recognised" in {
        val invalidMessageCode = JsString("InvalidMessageCode")

        invalidMessageCode.validate[MessageType] mustEqual JsError("Not a recognised value")
      }

      "returns an error when the message type code is not a string" in {
        val invalidMessageCode = JsNumber(1)

        invalidMessageCode.validate[MessageType] mustEqual JsError("Invalid type. Expected a JsString got a class play.api.libs.json.JsNumber")
      }

    }
  }

  "ordering" - {
    "comparing to DepartureDeclaration" - {
      "all status must have greater order" in {

        MessageTypeesExcluding(DepartureDeclaration).foreach {
          status =>
            val result = Ordering[MessageType].max(DepartureDeclaration, status)

            result mustBe status
        }
      }
    }

    "comparing to DeclarationRejected" - {
      "all status must have greater order except for DepartureDeclaration" in {

        MessageTypeesExcluding(DeclarationRejected, DepartureDeclaration, PositiveAcknowledgement).foreach {
          status =>
            val result = Ordering[MessageType].max(DeclarationRejected, status)

            result mustBe status
        }
      }

      "then it should have great order than PositiveAcknowledgement" in {
        val result = Ordering[MessageType].max(DeclarationRejected, PositiveAcknowledgement)

        result mustBe DeclarationRejected
      }
    }

    "comparing to PositiveAcknowledgement" - {
      "all status must have greater order except for DepartureDeclaration" in {

        MessageTypeesExcluding(PositiveAcknowledgement, DepartureDeclaration).foreach {
          status =>
            val result = Ordering[MessageType].max(PositiveAcknowledgement, status)

            result mustBe status
        }
      }
    }

    "comparing to MrnAllocated" - {
      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected
      )

      val greaterOrderValues = Seq(
        ControlDecisionNotification,
        NoReleaseForTransit,
        ReleaseForTransit,
        DeclarationCancellationRequest,
        CancellationDecision,
        WriteOffNotification,
        GuaranteeNotValid,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than DepartureDeclaration, PositiveAcknowledgement, DeclarationRejected" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(MrnAllocated, status)

            result mustBe MrnAllocated
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(MrnAllocated, status)

            result mustBe status
        }
      }
    }

    "comparing to ControlDecision" - {

      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated,
        GuaranteeNotValid
      )

      val greaterOrderValues = Seq(
        ControlDecisionNotification,
        ReleaseForTransit,
        DeclarationCancellationRequest,
        CancellationDecision,
        WriteOffNotification,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than DepartureDeclaration, PositiveAcknowledgement, DeclarationRejected, MrnAllocated, GuaranteeNotValid" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(ControlDecisionNotification, status)

            result mustBe ControlDecisionNotification
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(ControlDecisionNotification, status)

            result mustBe status
        }
      }
    }

    "comparing to GuaranteeNotValid" - {

      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated
      )

      val greaterOrderValues = Seq(
        ControlDecisionNotification,
        ReleaseForTransit,
        DeclarationCancellationRequest,
        CancellationDecision,
        WriteOffNotification,
        GuaranteeNotValid,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than DepartureDeclaration, PositiveAcknowledgement, DeclarationRejected, MrnAllocated" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(GuaranteeNotValid, status)

            result mustBe GuaranteeNotValid
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(GuaranteeNotValid, status)

            result mustBe status
        }
      }
    }

    "comparing to NoReleaseForTransit" - {
      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated,
        ControlDecisionNotification,
        GuaranteeNotValid
      )

      val greaterOrderValues = Seq(
        ReleaseForTransit,
        DeclarationCancellationRequest,
        CancellationDecision,
        WriteOffNotification,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than DepartureDeclaration, PositiveAcknowledgement, DeclarationRejected, MrnAllocated, ControlDecision, GuaranteeNotValid" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(NoReleaseForTransit, status)

            result mustBe NoReleaseForTransit
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(NoReleaseForTransit, status)

            result mustBe status
        }
      }
    }

    "comparing to ReleaseForTransit" - {
      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated,
        NoReleaseForTransit,
        ControlDecisionNotification,
        GuaranteeNotValid
      )

      val greaterOrderValues = Seq(
        ReleaseForTransit,
        DeclarationCancellationRequest,
        CancellationDecision,
        WriteOffNotification,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than DepartureDeclaration, PositiveAcknowledgement, DeclarationRejected, MrnAllocated, NoReleaseForTransit, Control Decision, GuaranteeNotValid" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(ReleaseForTransit, status)

            result mustBe ReleaseForTransit
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(ReleaseForTransit, status)

            result mustBe status
        }
      }
    }

    "comparing to DeclarationCancellationRequest" - {
      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated,
        NoReleaseForTransit,
        ControlDecisionNotification,
        GuaranteeNotValid,
        ReleaseForTransit
      )

      val greaterOrderValues = Seq(
        DeclarationCancellationRequest,
        CancellationDecision,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than " +
        "DepartureDeclaration, " +
        "PositiveAcknowledgement, " +
        "DeclarationRejected, " +
        "MrnAllocated, " +
        "NoReleaseForTransit, " +
        "Control Decision, " +
        "GuaranteeNotValid, " +
        "ReleaseForTransit, " +
        "WriteOffNotification" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(DeclarationCancellationRequest, status)

            result mustBe DeclarationCancellationRequest
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(DeclarationCancellationRequest, status)

            result mustBe status
        }
      }
    }

    "comparing to CancellationDecision" - {
      val lesserOrderValues = Seq(
        DepartureDeclaration,
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated,
        NoReleaseForTransit,
        ControlDecisionNotification,
        GuaranteeNotValid,
        ReleaseForTransit,
        DeclarationCancellationRequest
      )

      val greaterOrderValues = Seq(
        CancellationDecision,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than " +
        "DepartureDeclaration, " +
        "PositiveAcknowledgement, " +
        "DeclarationRejected, " +
        "MrnAllocated, " +
        "NoReleaseForTransit, " +
        "Control Decision, " +
        "GuaranteeNotValid, " +
        "ReleaseForTransit, " +
        "WriteOffNotification ," +
        "DeclarationCancellationRequest" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(CancellationDecision, status)

            result mustBe CancellationDecision
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(CancellationDecision, status)

            result mustBe status
        }
      }
    }

    "comparing to XMLSubmissionNegativeAcknowledgement" - {
      val lesserOrderValues = Seq(
        DepartureDeclaration,
        DeclarationCancellationRequest
      )

      val greaterOrderValues = Seq(
        PositiveAcknowledgement,
        DeclarationRejected,
        MrnAllocated,
        NoReleaseForTransit,
        ControlDecisionNotification,
        GuaranteeNotValid,
        ReleaseForTransit,
        CancellationDecision,
        XMLSubmissionNegativeAcknowledgement
      )

      "is greater order than " +
        "DepartureDeclaration, " +
        "DeclarationCancellationRequest" in {

        forAll(Gen.oneOf(lesserOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(XMLSubmissionNegativeAcknowledgement, status)

            result mustBe XMLSubmissionNegativeAcknowledgement
        }
      }

      "in lesser order than any other status" in {

        forAll(Gen.oneOf(greaterOrderValues)) {
          status =>
            val result = Ordering[MessageType].max(XMLSubmissionNegativeAcknowledgement, status)

            result mustBe status
        }
      }
    }

    "comparing to WriteOffNotification" - {

      "is greater order than all other status" in {

        forAll(Gen.oneOf(MessageType.values)) {
          status =>
            val result = Ordering[MessageType].max(WriteOffNotification, status)

            result mustBe WriteOffNotification
        }
      }
    }
  }

  def MessageTypeesExcluding(exclude: MessageType*): Seq[MessageType] =
    MessageType.values.filterNot(
      x => exclude.toSet.contains(x)
    )
}
