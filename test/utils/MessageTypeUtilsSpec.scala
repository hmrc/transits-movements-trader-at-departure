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

import java.time.LocalDateTime

import base.SpecBase
import generators.ModelGenerators
import models.DepartureStatus
import models.MessageMetaData
import models.MessageType
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MessageTypeUtilsSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "toDepartureStatus" - {
    "PositiveAcknowledgement must convert to PositiveAcknowledgement" in {
      MessageTypeUtils.toDepartureStatus(MessageType.PositiveAcknowledgement) mustBe DepartureStatus.PositiveAcknowledgement
    }

    "DepartureDeclaration must convert to DepartureSubmitted" in {
      MessageTypeUtils.toDepartureStatus(MessageType.DepartureDeclaration) mustBe DepartureStatus.DepartureSubmitted
    }

    "MrnAllocated must convert to MrnAllocated" in {
      MessageTypeUtils.toDepartureStatus(MessageType.MrnAllocated) mustBe DepartureStatus.MrnAllocated
    }

    "DeclarationRejected must convert to DepartureRejected" in {
      MessageTypeUtils.toDepartureStatus(MessageType.DeclarationRejected) mustBe DepartureStatus.DepartureRejected
    }

    "ControlDecisionNotification must convert to ControlDecisionNotification" in {
      MessageTypeUtils.toDepartureStatus(MessageType.ControlDecisionNotification) mustBe DepartureStatus.ControlDecisionNotification
    }

    "NoReleaseForTransit must convert to NoReleaseForTransit" in {
      MessageTypeUtils.toDepartureStatus(MessageType.NoReleaseForTransit) mustBe DepartureStatus.NoReleaseForTransit
    }

    "ReleaseForTransit must convert to ReleaseForTransit" in {
      MessageTypeUtils.toDepartureStatus(MessageType.ReleaseForTransit) mustBe DepartureStatus.ReleaseForTransit
    }

    "DeclarationCancellationRequest must convert to DeclarationCancellationRequest" in {
      MessageTypeUtils.toDepartureStatus(MessageType.DeclarationCancellationRequest) mustBe DepartureStatus.DeclarationCancellationRequest
    }

    "CancellationDecision must convert to CancellationDecision" in {
      MessageTypeUtils.toDepartureStatus(MessageType.CancellationDecision) mustBe DepartureStatus.CancellationDecision
    }

    "WriteOffNotification must convert to WriteOffNotification" in {
      MessageTypeUtils.toDepartureStatus(MessageType.WriteOffNotification) mustBe DepartureStatus.WriteOffNotification
    }

    "GuaranteeNotValid must convert to GuaranteeNotValid" in {
      MessageTypeUtils.toDepartureStatus(MessageType.GuaranteeNotValid) mustBe DepartureStatus.GuaranteeNotValid
    }

    "XMLSubmissionNegativeAcknowledgement must convert to DeclarationCancellationRequestNegativeAcknowledgement" in {
      MessageTypeUtils.toDepartureStatus(MessageType.XMLSubmissionNegativeAcknowledgement) mustBe DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
    }
  }

  "currentStatus" - {
    "when there is only the message from the user" - {

      "must return messageType" in {
        val localDateTime: LocalDateTime = LocalDateTime.now()

        val messages =
          List(
            MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime.minusSeconds(10))
          )

        MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.DepartureSubmitted
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

          MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.PositiveAcknowledgement
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

            MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated
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

              MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated
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

              MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.NoReleaseForTransit
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

              MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.CancellationDecision
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

            MessageTypeUtils.currentDepartureStatus(messages) mustBe DepartureStatus.MrnAllocated

          }
        }
      }
    }
  }

  "previousStatus" - {

    "when there is only the message from the user" - {

      "must return messageType" in {
        val localDateTime: LocalDateTime = LocalDateTime.now()

        val messages =
          List(
            MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime.minusSeconds(10))
          )

        val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
        MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.DepartureSubmitted
      }
    }

    "when there are responses from NCTS for the departure" - {
      "when there is a single response from NCTS" - {
        "must return the messageType for the second latest NCTS message" in {

          val localDateTime: LocalDateTime = LocalDateTime.now()

          val messages =
            List(
              MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
              MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(10))
            )

          val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
          MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.DepartureSubmitted
        }
      }

      "when there are multiple responses from NCTS" - {
        "when messages are well ordered" - {
          "must return the messageType for the second latest NCTS message" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val messages =
              List(
                MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(10)),
                MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusSeconds(20))
              )

            val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
            MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.PositiveAcknowledgement
          }
        }

        "when messages are not well ordered" - {
          "must return the messageType for the message with the second latest dateTime" - {

            "Scenario 1" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val messages =
                List(
                  MessageMetaData(messageType = MessageType.DepartureDeclaration, dateTime = localDateTime),
                  MessageMetaData(messageType = MessageType.MrnAllocated, dateTime = localDateTime.plusSeconds(20)),
                  MessageMetaData(messageType = MessageType.PositiveAcknowledgement, dateTime = localDateTime.plusSeconds(10))
                )

              val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
              MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.PositiveAcknowledgement
            }

            "Scenario 2" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val messages =
                List(
                  MessageMetaData(messageType = MessageType.GuaranteeNotValid, localDateTime.plusDays(3)),
                  MessageMetaData(messageType = MessageType.NoReleaseForTransit, localDateTime.plusDays(4)),
                  MessageMetaData(messageType = MessageType.DepartureDeclaration, localDateTime),
                  MessageMetaData(messageType = MessageType.MrnAllocated, localDateTime.plusDays(2)),
                  MessageMetaData(messageType = MessageType.PositiveAcknowledgement, localDateTime.plusDays(1))
                )

              val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
              MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.GuaranteeNotValid
            }

            "Scenario 3" in {

              val localDateTime: LocalDateTime = LocalDateTime.now()

              val messages =
                List(
                  MessageMetaData(messageType = MessageType.DeclarationCancellationRequest, localDateTime.plusWeeks(3)),
                  MessageMetaData(messageType = MessageType.DepartureDeclaration, localDateTime),
                  MessageMetaData(messageType = MessageType.CancellationDecision, localDateTime.plusMonths(4)),
                  MessageMetaData(messageType = MessageType.MrnAllocated, localDateTime.plusDays(2)),
                  MessageMetaData(messageType = MessageType.PositiveAcknowledgement, localDateTime.plusSeconds(1))
                )

              val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
              MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.DeclarationCancellationRequest
            }
          }
        }

        "when messages have the same latest dateTime" - {

          "must return the second latest messageType" in {

            val localDateTime: LocalDateTime = LocalDateTime.now()

            val messages =
              List(
                MessageMetaData(messageType = MessageType.DepartureDeclaration, localDateTime),
                MessageMetaData(messageType = MessageType.MrnAllocated, localDateTime),
                MessageMetaData(messageType = MessageType.PositiveAcknowledgement, localDateTime.plusMinutes(10))
              )

            val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
            MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.MrnAllocated
          }
        }
      }
    }

    "when currentStatus is DeclarationCancellationRequestNegativeAcknowledgement" - {

      "must not weight previous messages when messageType is DepartureSubmitted" in {

        val localDateTime: LocalDateTime = LocalDateTime.now()

        val messages =
          List(
            MessageMetaData(messageType = MessageType.DepartureDeclaration, localDateTime),
            MessageMetaData(messageType = MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime)
          )

        val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
        currentStatus mustBe DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
        MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.DepartureSubmitted
      }

      "must not weight previous messages when messageType is DepartureCancellation" in {

        val localDateTime: LocalDateTime = LocalDateTime.now()

        val messages =
          List(
            MessageMetaData(messageType = MessageType.DeclarationCancellationRequest, localDateTime),
            MessageMetaData(messageType = MessageType.XMLSubmissionNegativeAcknowledgement, localDateTime)
          )

        val currentStatus = MessageTypeUtils.currentDepartureStatus(messages)
        currentStatus mustBe DepartureStatus.DeclarationCancellationRequestNegativeAcknowledgement
        MessageTypeUtils.previousDepartureStatus(messages, currentStatus) mustBe DepartureStatus.DeclarationCancellationRequest
      }
    }
  }

}
