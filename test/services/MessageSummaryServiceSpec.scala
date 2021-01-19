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

package services
import base.SpecBase
import cats.data.NonEmptyList
import generators.ModelGenerators
import models.MessageStatus.SubmissionPending
import models.MessageStatus.SubmissionSucceeded
import models.MessageType.CancellationDecision
import models.MessageType.DeclarationCancellationRequest
import models.MessageType.DeclarationRejected
import models.MessageType.DepartureDeclaration
import models.MessageType.GuaranteeNotValid
import models.MessageType.MrnAllocated
import models._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MessageSummaryServiceSpec extends SpecBase with ModelGenerators with ScalaCheckDrivenPropertyChecks {

  import MessageSummaryServiceSpec.MovementMessagesHelpers._

  def messageGeneratorSent(messageType: MessageType): Gen[MessageWithStatus] = {
    val message = xml.XML.loadString(s"<${messageType.rootNode}>test</${messageType.rootNode}>")
    arbitrary[MessageWithStatus].map(_.copy(messageType = messageType, message = message, status = SubmissionPending))
  }

  def messageGeneratorResponse(messageType: MessageType): Gen[MessageWithoutStatus] = {
    val message = xml.XML.loadString(s"<${messageType.rootNode}>test</${messageType.rootNode}>")
    arbitrary[MessageWithoutStatus].map(_.copy(messageType = messageType, message = message))
  }

  def createMovement(msgs: NonEmptyList[Message]): Gen[Departure] =
    for {
      departureMovement <- arbitrary[Departure]
    } yield departureMovement.copy(messages = msgs)

  private val ie015Gen = messageGeneratorSent(DepartureDeclaration)
  private val ie016Gen = messageGeneratorResponse(DeclarationRejected)
  private val ie028Gen = messageGeneratorResponse(MrnAllocated)
  private val ie055Gen = messageGeneratorResponse(GuaranteeNotValid)
  private val ie009Gen = messageGeneratorResponse(CancellationDecision)
  private val ie014Gen = messageGeneratorResponse(DeclarationCancellationRequest)

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
                messageId mustEqual MessageId.fromMessageIdValue(1).value
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
                messageId mustEqual MessageId.fromMessageIdValue(1).value
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
                messageId mustEqual MessageId.fromMessageIdValue(3).value
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
                messageId mustEqual MessageId.fromMessageIdValue(3).value
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
                messageId mustEqual MessageId.fromMessageIdValue(2).value
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
                messageId mustEqual MessageId.fromMessageIdValue(3).value
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
                messageId mustEqual MessageId.fromMessageIdValue(2).value
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
                messageId mustEqual MessageId.fromMessageIdValue(4).value
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
                service.declarationRejectionMessage(departure) must not be defined

            }
        }
      }

      "latest IE055 when there is only an IE015 and a IE055" in {
        forAll(ie015Gen, ie055Gen) {
          (ie015, ie055) =>
            forAll(createMovement(NonEmptyList.of(ie015, ie055))) {
              departure =>
                val (message, messageId) = service.guaranteeNotValidRejectionMessage(departure).value

                message mustEqual ie055
                messageId mustEqual MessageId.fromMessageIdValue(2).value
            }
        }
      }

      "None when there has been a guarantee not valid message and correction" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(1), ie015Gen.msgCorrId(2)) {
          case (ie015Old, ie055, ie015) =>
            val messages = NonEmptyList.of(ie015Old, ie055, ie015)

            forAll(createMovement(messages)) {
              departure =>
                service.guaranteeNotValidRejectionMessage(departure) must not be defined
            }
        }
      }

      "IE015 when all IE055 have been rejected" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(1), ie015Gen.msgCorrId(2), ie055Gen.msgCorrId(2)) {
          case (ie015Old, ie055Old, ie015, ie055) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie055Old, ie015, ie055))) {
              departure =>
                val (message, messageId) = service.guaranteeNotValidRejectionMessage(departure).value

                message mustEqual ie055
                messageId mustEqual MessageId.fromMessageIdValue(4).value
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
                messageId mustEqual MessageId.fromMessageIdValue(2).value
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
                messageId mustEqual MessageId.fromMessageIdValue(3).value
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
                messageId mustEqual MessageId.fromMessageIdValue(2).value
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
                messageId mustEqual MessageId.fromMessageIdValue(3).value
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
                service.messagesSummary(departure) mustEqual MessagesSummary(departure, MessageId.fromMessageIdValue(1).value, None)

            }
        }
      }

      "latest IE016 when there is only an IE015 and a IE016" in {
        forAll(ie015Gen, ie016Gen) {
          (ie015, IE016) =>
            val messages = NonEmptyList.of(ie015, IE016)

            forAll(createMovement(messages)) {
              departure =>
                val expectedMessageSummary = MessagesSummary(departure, MessageId.fromMessageIdValue(1).value, MessageId.fromMessageIdValue(2))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }

      "latest IE016 when there has been an IE015 correction" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(2), ie015Gen.msgCorrId(3)) {
          case (ie015Old, ie016Old, ie015) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016Old, ie015))) {
              departure =>
                val expectedMessageSummary = MessagesSummary(departure, MessageId.fromMessageIdValue(3).value, None)

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }

      "IE016 when all IE015 have been rejected" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie016Gen.msgCorrId(2), ie015Gen.msgCorrId(3), ie016Gen.msgCorrId(4)) {
          case (ie015Old, ie016Old, ie015, ie016) =>
            forAll(createMovement(NonEmptyList.of(ie015Old, ie016Old, ie015, ie016))) {
              departure =>
                val expectedMessageSummary = MessagesSummary(departure, MessageId.fromMessageIdValue(3).value, MessageId.fromMessageIdValue(4))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }

      }

      "IE015 and IE028" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie028Gen.msgCorrId(2)) {
          case (ie015, ie0028) =>
            val messages = NonEmptyList.of(ie015, ie0028)

            forAll(createMovement(messages)) {
              departure =>
                val expectedMessageSummary =
                  MessagesSummary(departure, MessageId.fromMessageIdValue(1).value, None, MessageId.fromMessageIdValue(2))

                service.messagesSummary(departure) mustEqual expectedMessageSummary
            }
        }
      }

      "IE015 and IE055" in {
        forAll(ie015Gen.submitted.msgCorrId(1), ie055Gen.msgCorrId(2)) {
          case (ie015, ie055) =>
            val messages = NonEmptyList.of(ie015, ie055)

            forAll(createMovement(messages)) {
              departure =>
                val expectedMessageSummary =
                  MessagesSummary(departure, MessageId.fromMessageIdValue(1).value, None, None, MessageId.fromMessageIdValue(2))

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
