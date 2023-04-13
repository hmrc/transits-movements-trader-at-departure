/*
 * Copyright 2023 HM Revenue & Customs
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

package services

import java.time.LocalDateTime

import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.MessageType.CancellationDecision
import models.MessageType.ControlDecisionNotification
import models.MessageType.DeclarationCancellationRequest
import models.MessageType.DeclarationRejected
import models.MessageType.DepartureDeclaration
import models.MessageType.GuaranteeNotValid
import models.MessageType.MrnAllocated
import models.MessageType.NoReleaseForTransit
import models._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MessageSummaryServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  import MessageSummaryServiceSpec.MovementMessagesHelpers._

  def messageGeneratorSent(messageType: MessageType, dateTime: LocalDateTime): Gen[MessageWithStatus] = {
    val message = xml.XML.loadString(s"<${messageType.rootNode}>test</${messageType.rootNode}>")
    arbitrary[MessageWithStatus].map(_.copy(messageType = messageType, message = message, status = SubmissionPending, dateTime = dateTime))
  }

  def messageGeneratorResponse(messageType: MessageType, dateTime: LocalDateTime): Gen[MessageWithoutStatus] = {
    val message = xml.XML.loadString(s"<${messageType.rootNode}>test</${messageType.rootNode}>")
    arbitrary[MessageWithoutStatus].map(_.copy(messageType = messageType, message = message, dateTime = dateTime))
  }

  def createMovementWithState(msgs: NonEmptyList[Message]): Gen[Departure] =
    for {
      departureMovement <- arbitrary[Departure]
    } yield departureMovement.copy(messages = msgs)

  def createMovement(msgs: NonEmptyList[Message]): Gen[Departure] =
    for {
      departureMovement <- arbitrary[Departure]
    } yield departureMovement.copy(messages = msgs)

  private val localDateTime  = LocalDateTime.now()
  private val updateDateTime = localDateTime.plusDays(1)
  private val ie015Gen       = messageGeneratorSent(DepartureDeclaration, localDateTime)
  private val ie016Gen       = messageGeneratorResponse(DeclarationRejected, updateDateTime)
  private val ie028Gen       = messageGeneratorResponse(MrnAllocated, updateDateTime)
  private val ie055Gen       = messageGeneratorResponse(GuaranteeNotValid, updateDateTime)
  private val ie009Gen       = messageGeneratorResponse(CancellationDecision, updateDateTime)
  private val ie014Gen       = messageGeneratorResponse(DeclarationCancellationRequest, updateDateTime)
  private val ie051Gen       = messageGeneratorResponse(NoReleaseForTransit, updateDateTime)
  private val ie060Gen       = messageGeneratorResponse(ControlDecisionNotification, updateDateTime)
  private val ie917Gen       = messageGeneratorResponse(MessageType.XMLSubmissionNegativeAcknowledgement, updateDateTime)

  private val service = new MessageSummaryService

  "declarationMessage" - {

    "must return" - {

      "the original IE015 when there have been no other messages" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                val (message, messageId) = service.declarationMessage(departure)
                message mustEqual ie015
                messageId mustEqual ie015.messageId
            }
        }
      }

      "the original IE015 and first IE016 when there is only an IE015 and a IE016" in {
        forAll(ie015Gen, ie016Gen) {
          (ie015, ie016) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie016))) {
              departure =>
                val (message, messageId) = service.declarationMessage(departure)
                message mustEqual ie015
                messageId mustEqual ie015.messageId
            }
        }
      }

      "the new IE015 when there is multiple IE015 messages" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(1), ie015Gen.msgCorrId(2)) {
          case (ie015Old, ie016, ie015) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016, ie015))) {
              departure =>
                val (message, messageId) = service.declarationMessage(departure)
                message mustEqual ie015
                messageId mustEqual ie015.messageId
            }
        }
      }

      "the latest IE015 when all IE015 have been rejected" in {
        val service = new MessageSummaryService

        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(1), ie015Gen.msgCorrId(2), ie016Gen.msgCorrId(2)) {
          case (ie015Old, ie016Old, ie015, ie016) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016Old, ie015, ie016))) {
              departure =>
                val (message, messageId) = service.declarationMessage(departure)
                message mustEqual ie015
                messageId mustEqual ie015.messageId
            }
        }
      }
    }
  }

  "mrnAllocatedMessage" - {

    "must return" - {

      "None when IE028 does not exit" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                service.mrnAllocatedMessage(departure) must not be defined
            }
        }
      }

      "None when IE015 does not exit" in {
        forAll(ie028Gen) {
          ie028 =>
            forAll(createMovement(NonEmptyList.one(ie028))) {
              departure =>
                service.mrnAllocatedMessage(departure) must not be defined
            }
        }
      }

      "IE028 when a IE015 and IE028 exist" in {
        forAll(ie015Gen, ie028Gen) {
          (ie015, ie028) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie028))) {
              departure =>
                val (message, messageId) = service.mrnAllocatedMessage(departure).value

                message mustEqual ie028
                messageId mustEqual ie028.messageId
            }
        }
      }
      //This should never happen
      "latest IE028 when multiple IE015 messages exist" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie028Gen.msgCorrId(2), ie028Gen.msgCorrId(3)) {
          case (ie015, ie028Old, ie028) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie028Old, ie028))) {
              departure =>
                val (message, messageId) = service.mrnAllocatedMessage(departure).value

                message mustEqual ie028
                messageId mustEqual ie028.messageId
            }
        }
      }
    }
  }

  "declarationRejectionMessage" - {

    "must return" - {
      "None when there are none in the movement" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                service.declarationRejectionMessage(departure) must not be defined

            }
        }
      }

      "latest IE016 when there is only an IE015 and a IE016" in {
        forAll(ie015Gen, ie016Gen) {
          (ie015, ie016) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie016))) {
              departure =>
                val (message, messageId) = service.declarationRejectionMessage(departure).value

                message mustEqual ie016
                messageId mustEqual ie016.messageId
            }
        }
      }

      "None when there has been a rejected declaration and correction" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(1), ie015Gen.msgCorrId(2)) {
          case (ie015Old, ie016, ie015) =>
            val messages = NonEmptyList.of(ie015Old, ie016, ie015)

            forAll(createMovement(messages)) {
              departure =>
                service.declarationRejectionMessage(departure) must not be defined
            }
        }
      }

      "IE015 when all IE016 have been rejected" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(1), ie015Gen.msgCorrId(2), ie016Gen.msgCorrId(2)) {
          case (ie015Old, ie016Old, ie015, ie016) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016Old, ie015, ie016))) {
              departure =>
                val (message, messageId) = service.declarationRejectionMessage(departure).value

                message mustEqual ie016
                messageId mustEqual ie016.messageId
            }
        }
      }
    }
  }

  "guaranteeNotValidMessage" - {

    "must return" - {
      "None when there are none in the movement" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                service.guaranteeNotValidRejectionMessage(departure) must not be defined

            }
        }
      }

      "latest IE055 when thee departure state is GuaranteeNotValid" in {

        forAll(ie015Gen, ie055Gen) {
          (ie015, ie055) =>
            forAll(createMovementWithState(NonEmptyList.of(ie015, ie055))) {
              departure =>
                val (message, messageId) = service.guaranteeNotValidRejectionMessage(departure).value

                message mustEqual ie055
                messageId mustEqual ie055.messageId
            }
        }
      }

      "None when there has been a guarantee not valid message and correction" in {
        val ie015GenNew = messageGeneratorSent(DepartureDeclaration, localDateTime.plusDays(2))
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(1), ie015GenNew.msgCorrId(2)) {
          case (ie015Old, ie055, ie015) =>
            val messages = NonEmptyList.of(ie015Old, ie055, ie015)

            forAll(createMovementWithState(messages)) {
              departure =>
                service.guaranteeNotValidRejectionMessage(departure) must not be defined
            }
        }
      }

      "GuaranteeNotValid message when multiple guarantee not valid messages and a single departure" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(1), ie055Gen.msgCorrId(2)) {
          case (ie015Old, ie055, secondIE055) =>
            val messages = NonEmptyList.of(ie015Old, ie055, secondIE055)

            forAll(createMovementWithState(messages)) {
              departure =>
                val (message, messageId) = service.guaranteeNotValidRejectionMessage(departure).value

                message mustBe secondIE055
                messageId mustBe secondIE055.messageId
            }
        }
      }

      "IE015 when all IE055 have been rejected" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(1), ie015Gen.msgCorrId(2), ie055Gen.msgCorrId(2)) {
          case (ie015Old, ie055Old, ie015, ie055) =>
            forAll(createMovementWithState(NonEmptyList.of(ie015Old, ie055Old, ie015, ie055))) {
              departure =>
                val (message, messageId) = service.guaranteeNotValidRejectionMessage(departure).value

                message mustEqual ie055
                messageId mustEqual ie055.messageId
            }
        }
      }
    }
  }

  "cancellationDecisionMessage" - {

    "must return" - {
      "None when there are no IE015 in the movement" in {
        forAll(ie009Gen) {
          ie009 =>
            forAll(createMovement(NonEmptyList.one(ie009))) {
              departure =>
                service.cancellationDecisionMessage(departure) must not be defined
            }
        }
      }

      "latest IE009 when there is only an IE015 and a IE009" in {
        forAll(ie015Gen, ie009Gen) {
          (ie015, ie009) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie009))) {
              departure =>
                val (message, messageId) = service.cancellationDecisionMessage(departure).value
                message mustEqual ie009
                messageId mustEqual ie009.messageId
            }
        }
      }

      "Get the latest IE009 when there are multiples" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie009Gen.msgCorrId(1), ie009Gen.msgCorrId(2)) {
          case (ie015Old, ie009Old, ie009) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie009Old, ie009))) {
              departure =>
                val (message, messageId) = service.cancellationDecisionMessage(departure).value

                message mustEqual ie009
                messageId mustEqual ie009.messageId
            }
        }
      }
    }
  }

  "cancellationRequestMessage" - {

    "must return" - {
      "None when there are no IE015 in the movement" in {
        forAll(ie014Gen) {
          ie014 =>
            forAll(createMovement(NonEmptyList.one(ie014))) {
              departure =>
                service.declarationCancellationRequestMessage(departure) must not be defined
            }
        }
      }

      "latest IE014 when there is only an IE015 and a IE014" in {
        forAll(ie015Gen, ie014Gen) {
          (ie015, ie014) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie014))) {
              departure =>
                val (message, messageId) = service.declarationCancellationRequestMessage(departure).value
                message mustEqual ie014
                messageId mustEqual ie014.messageId
            }
        }
      }

      "Get the latest IE014 when there are multiples" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie014Gen.msgCorrId(1), ie014Gen.msgCorrId(2)) {
          case (ie015Old, ie014Old, ie014) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie014Old, ie014))) {
              departure =>
                val (message, messageId) = service.declarationCancellationRequestMessage(departure).value

                message mustEqual ie014
                messageId mustEqual ie014.messageId
            }
        }
      }
    }
  }

  "noReleaseForTransitMessage" - {

    "must return" - {
      "None when there are none in the movement" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                service.noReleaseForTransitMessage(departure) must not be defined

            }
        }
      }

      "latest IE051 when there is only an IE015 and a IE051" in {
        forAll(ie015Gen, ie051Gen) {
          (ie015, ie051) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie051))) {
              departure =>
                val (message, messageId) = service.noReleaseForTransitMessage(departure).value

                message mustEqual ie051
                messageId mustEqual ie051.messageId
            }
        }
      }

      "Get the latest IE051 when there are multiples" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie051Gen.msgCorrId(1), ie051Gen.msgCorrId(2)) {
          case (ie015Old, ie051Old, ie051) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie051Old, ie051))) {
              departure =>
                val (message, messageId) = service.noReleaseForTransitMessage(departure).value

                message mustEqual ie051
                messageId mustEqual ie051.messageId
            }
        }
      }

    }
  }

  "controlDecisionMessage" - {

    "must return" - {
      "None when there are none in the movement" in {
        forAll(ie060Gen) {
          ie060 =>
            forAll(createMovement(NonEmptyList.one(ie060))) {
              departure =>
                service.controlDecisionMessage(departure) must not be defined

            }
        }
      }

      "latest IE060 when there is only an IE015 and a IE060" in {
        forAll(ie015Gen, ie060Gen) {
          (ie015, ie060) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie060))) {
              departure =>
                val (message, messageId) = service.controlDecisionMessage(departure).value

                message mustEqual ie060
                messageId mustEqual ie060.messageId
            }
        }
      }

      "Get the latest IE060 when there are multiples" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie060Gen.msgCorrId(1), ie060Gen.msgCorrId(2)) {
          case (ie015Old, ie060Old, ie060) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie060Old, ie060))) {
              departure =>
                val (message, messageId) = service.controlDecisionMessage(departure).value

                message mustEqual ie060
                messageId mustEqual ie060.messageId
            }
        }
      }

    }
  }

  "xmlSubmissionNegativeAcknowledgement" - {

    "must return" - {
      "None when there are none in the movement" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                service.xmlSubmissionNegativeAcknowledgementMessage(departure) must not be defined
            }
        }
      }

      "latest IE917 when there is only an IE015 and a IE917" in {
        forAll(ie015Gen, ie917Gen) {
          (ie015, ie917) =>
            val messages = NonEmptyList.of(ie015, ie917)

            forAll(createMovement(messages)) {
              departure =>
                val (message, messageId) = service.xmlSubmissionNegativeAcknowledgementMessage(departure).value

                message mustEqual ie917
                messageId mustEqual ie917.messageId
            }
        }
      }

      "latest IE917 when there are multiple rejected IE014" in {
        forAll(ie015Gen, ie014Gen.msgCorrId(1), ie917Gen.msgCorrId(1), ie014Gen.msgCorrId(2), ie917Gen.msgCorrId(2)) {
          (ie015, ie014Old, ie917Old, ie014, ie917) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie014Old, ie917Old, ie014, ie917))) {
              departure =>
                val (message, messageId) = service.xmlSubmissionNegativeAcknowledgementMessage(departure).value
                message mustEqual ie917
                messageId mustEqual ie917.messageId
            }
        }
      }
    }
  }

  "messagesSummary" - {

    "must return" - {

      "initial IE015 and no IE016 when there is only a IE015" in {
        forAll(ie015Gen) {
          ie015 =>
            forAll(createMovement(NonEmptyList.one(ie015))) {
              departure =>
                service.messagesSummary(departure) mustEqual MessagesSummary(departure, ie015.messageId, None)

            }
        }
      }

      "latest IE016 when there is only an IE015 and a IE016" in {
        forAll(ie015Gen, ie016Gen) {
          (ie015, ie016) =>
            val messages = NonEmptyList.of(ie015, ie016)

            forAll(createMovement(messages)) {
              departure =>
                val expectedMessageSummary = MessagesSummary(departure, ie015.messageId, Some(ie016.messageId))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }

      "latest IE016 when there has been an IE015 correction" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(2), ie015Gen.msgCorrId(3)) {
          case (ie015Old, ie016Old, ie015) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016Old, ie015))) {
              departure =>
                val expectedMessageSummary = MessagesSummary(departure, ie015.messageId, None)

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }

      "IE016 when all IE015 have been rejected" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(2), ie015Gen.msgCorrId(3), ie016Gen.msgCorrId(4)) {
          case (ie015Old, ie016Old, ie015, ie016) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016Old, ie015, ie016))) {
              departure =>
                val expectedMessageSummary = MessagesSummary(departure, ie015.messageId, Some(ie016.messageId))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }

      }

      "IE015 and IE028" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie028Gen.msgCorrId(2)) {
          case (ie015, ie028) =>
            val messages = NonEmptyList.of(ie015, ie028)

            forAll(createMovement(messages)) {
              departure =>
                val expectedMessageSummary =
                  MessagesSummary(departure, ie015.messageId, None, Some(ie028.messageId))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }

      "IE015 and IE055" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(2)) {
          case (ie015, ie055) =>
            val messages = NonEmptyList.of(ie015, ie055)

            forAll(createMovementWithState(messages)) {
              departure =>
                val expectedMessageSummary =
                  MessagesSummary(departure, ie015.messageId, None, None, Some(ie055.messageId))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }
    }
  }
}

object MessageSummaryServiceSpec {

  object MovementMessagesHelpers {

    implicit class SubmittedOps(movementMessageWithStatus: Gen[MessageWithStatus]) {
      def submitted: Gen[MessageWithStatus] = movementMessageWithStatus.map(_.copy(status = SubmissionSucceeded))
    }

    implicit class MessageCorrelationIdOps(movementMessageWithStatus: Gen[MessageWithStatus]) {
      def msgCorrId(value: Int): Gen[Message] = movementMessageWithStatus.map(_.copy(messageCorrelationId = value))
    }

    implicit class MessageCorrelationIdOps2(movementMessageWithStatus: Gen[MessageWithoutStatus]) {
      def msgCorrId(value: Int): Gen[Message] = movementMessageWithStatus.map(_.copy(messageCorrelationId = value))
    }

  }

}
