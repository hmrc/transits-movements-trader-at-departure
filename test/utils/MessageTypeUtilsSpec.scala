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

package utils

import base.SpecBase
import generators.ModelGenerators
import models.DepartureStatus
import models.MessageMetaData
import models.MessageType
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.LocalDateTime

class MessageTypeUtilsSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "latestDepartureStatus" - {
    "when there is only the message from the user" - {

      "must return messageType" - {

        def runTest(input: MessageType, expectedOutput: DepartureStatus): Assertion = {
          val localDateTime: LocalDateTime = LocalDateTime.now()

          val messages =
            List(
              MessageMetaData(messageType = input, dateTime = localDateTime.minusSeconds(10))
            )

          MessageTypeUtils.latestDepartureStatus(messages) mustBe expectedOutput
        }

        "when PositiveAcknowledgement" in {
          runTest(MessageType.PositiveAcknowledgement, DepartureStatus.PositiveAcknowledgement)
        }

        "when DepartureDeclaration" in {
          runTest(MessageType.DepartureDeclaration, DepartureStatus.DepartureSubmitted)
        }

        "when MrnAllocated" in {
          runTest(MessageType.MrnAllocated, DepartureStatus.MrnAllocated)
        }

        "when DeclarationRejected" in {
          runTest(MessageType.DeclarationRejected, DepartureStatus.DepartureRejected)
        }

        "when ControlDecisionNotification" in {
          runTest(MessageType.ControlDecisionNotification, DepartureStatus.ControlDecisionNotification)
        }

        "when NoReleaseForTransit" in {
          runTest(MessageType.NoReleaseForTransit, DepartureStatus.NoReleaseForTransit)
        }

        "when ReleaseForTransit" in {
          runTest(MessageType.ReleaseForTransit, DepartureStatus.ReleaseForTransit)
        }

        "when DeclarationCancellationRequest" in {
          runTest(MessageType.DeclarationCancellationRequest, DepartureStatus.DeclarationCancellationRequest)
        }

        "when CancellationDecision" in {
          runTest(MessageType.CancellationDecision, DepartureStatus.CancellationDecision)
        }

        "when WriteOffNotification" in {
          runTest(MessageType.WriteOffNotification, DepartureStatus.WriteOffNotification)
        }

        "when GuaranteeNotValid" in {
          runTest(MessageType.GuaranteeNotValid, DepartureStatus.GuaranteeNotValid)
        }
      }
    }

    "when there are responses from NCTS for the departure" - {
      "when there is a single response from NCTS" - {
        "must return the messageType for the latest NCTS message" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val messages =
            List(
              MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
              MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(10))
            )

          MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.PositiveAcknowledgement
        }
      }

      "when there are multiple responses from NCTS" - {
        "when messages are well ordered" - {
          "must return the messageType for the latest NCTS message" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val messages =
              List(
                MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(10)),
                MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusSeconds(20))
              )

            MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated
          }
        }

        "when messages are not well ordered" - {
          "must return the messageType for the message with the latest dateTime" - {

            "Scenario 1" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val messages =
                List(
                  MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                  MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusSeconds(20)),
                  MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(10))
                )

              MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated
            }

            "Scenario 2" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val messages =
                List(
                  MessageMetaData(messageType = MessageType.GuaranteeNotValid, dateTime = localDateTime.plusDays(3)),
                  MessageMetaData(messageType = MessageType.NoReleaseForTransit, dateTime = localDateTime.plusDays(4)),
                  MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                  MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusDays(2)),
                  MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusDays(1))
                )

              MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.NoReleaseForTransit
            }

            "Scenario 3" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val messages =
                List(
                  MessageMetaData(messageType = MessageType.DeclarationCancellationRequest, dateTime = localDateTime.plusWeeks(3)),
                  MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                  MessageMetaData(messageType = MessageType.CancellationDecision, dateTime = localDateTime.plusMonths(4)),
                  MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusDays(2)),
                  MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(1))
                )

              MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.CancellationDecision
            }
          }
        }

        "when messages have the same latest dateTime" - {

          "must return the latest messageType" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val messages =
              List(
                MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusMinutes(10)),
                MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusMinutes(10))
              )

            MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated

          }
        }
      }
    }

    "when latest message type is XMLSubmissionNegativeAcknowledgement" - {

      "when previous message type is DepartureDeclaration" - {
        "must return DepartureSubmittedNegativeAcknowledgement" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val messages =
            List(
              MessageMetaData(messageType = MessageType.DepartureDeclaration, localDateTime),
              MessageMetaData(messageType = MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime.plusMinutes(10))
            )

          MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.DepartureSubmittedNegativeAcknowledgement
        }
      }

      "when previous message type is DeclarationCancellationRequest" - {
        "must return DeclarationCancellationRequestNegativeAcknowledgement" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val messages =
            List(
              MessageMetaData(messageType = MessageType.DeclarationCancellationRequest, localDateTime),
              MessageMetaData(messageType = MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime.plusMinutes(10))
            )

          MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
        }
      }

      "when previous message type is something else" - {
        "must return the corresponding departure status" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val messages =
            List(
              MessageMetaData(messageType = MessageType.MrnAllocated, localDateTime),
              MessageMetaData(messageType = MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime.plusMinutes(10))
            )

          MessageTypeUtils.latestDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated
        }
      }
    }
  }

}
