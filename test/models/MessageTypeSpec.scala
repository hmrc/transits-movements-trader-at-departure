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
import models.MessageType.CancellationDecision
import models.MessageType.ControlDecisionNotification
import models.MessageType.DeclarationCancellationRequest
import models.MessageType.DeclarationRejected
import models.MessageType.DepartureDeclaration
import models.MessageType.GuaranteeNotValid
import models.MessageType.MrnAllocated
import models.MessageType.NoReleaseForTransit
import models.MessageType.PositiveAcknowledgement
import models.MessageType.ReleaseForTransit
import models.MessageType.WriteOffNotification
import org.scalacheck.Arbitrary.arbitrary
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

}
