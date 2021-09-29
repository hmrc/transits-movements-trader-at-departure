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
import models.MessageType.ControlDecisionNotification
import models.MessageType.DeclarationRejected
import models.MessageType.MrnAllocated
import models.MessageType.NoReleaseForTransit
import models.MessageType.PositiveAcknowledgement
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.LocalDateTime

class LatestMessagesSpec extends SpecBase with ScalaCheckDrivenPropertyChecks with ModelGenerators {

  "LatestMessage" - {

    "fromMessageMetaData" - {

      "must if one message" in {

        val messageMetaData1 = MessageMetaData(PositiveAcknowledgement, LocalDateTime.now())

        val messageMetaDataList = Seq(messageMetaData1)

        val expectedResult = LatestMessages(messageMetaData1, None)

        LatestMessages.fromMessageMetaData(messageMetaDataList) mustBe expectedResult
      }

      "must order by dateTime and return the top 2 results by days" in {

        val messageMetaData1 = MessageMetaData(PositiveAcknowledgement, LocalDateTime.now())
        val messageMetaData2 = MessageMetaData(MrnAllocated, LocalDateTime.now().minusDays(1))
        val messageMetaData3 = MessageMetaData(DeclarationRejected, LocalDateTime.now().minusDays(2))
        val messageMetaData4 = MessageMetaData(ControlDecisionNotification, LocalDateTime.now().minusDays(3))
        val messageMetaData5 = MessageMetaData(NoReleaseForTransit, LocalDateTime.now().minusDays(4))

        val messageMetaDataList = Seq(messageMetaData3, messageMetaData5, messageMetaData1, messageMetaData2, messageMetaData4)

        val expectedResult = LatestMessages(messageMetaData1, Some(messageMetaData2))

        LatestMessages.fromMessageMetaData(messageMetaDataList) mustBe expectedResult
      }

      "must order by dateTime and return the top 2 results by seconds" in {

        val messageMetaData1 = MessageMetaData(PositiveAcknowledgement, LocalDateTime.now())
        val messageMetaData2 = MessageMetaData(MrnAllocated, LocalDateTime.now().minusSeconds(1))
        val messageMetaData3 = MessageMetaData(DeclarationRejected, LocalDateTime.now().minusSeconds(2))
        val messageMetaData4 = MessageMetaData(ControlDecisionNotification, LocalDateTime.now().minusSeconds(3))
        val messageMetaData5 = MessageMetaData(NoReleaseForTransit, LocalDateTime.now().minusSeconds(4))

        val messageMetaDataList = Seq(messageMetaData3, messageMetaData5, messageMetaData1, messageMetaData2, messageMetaData4)

        val expectedResult = LatestMessages(messageMetaData1, Some(messageMetaData2))

        LatestMessages.fromMessageMetaData(messageMetaDataList) mustBe expectedResult
      }

      "must order by dateTime and return the top 2 results by hours" in {

        val messageMetaData1 = MessageMetaData(PositiveAcknowledgement, LocalDateTime.now())
        val messageMetaData2 = MessageMetaData(MrnAllocated, LocalDateTime.now().minusHours(1))
        val messageMetaData3 = MessageMetaData(DeclarationRejected, LocalDateTime.now().minusHours(2))
        val messageMetaData4 = MessageMetaData(ControlDecisionNotification, LocalDateTime.now().minusHours(3))
        val messageMetaData5 = MessageMetaData(NoReleaseForTransit, LocalDateTime.now().minusHours(4))

        val messageMetaDataList = Seq(messageMetaData3, messageMetaData5, messageMetaData1, messageMetaData2, messageMetaData4)

        val expectedResult = LatestMessages(messageMetaData1, Some(messageMetaData2))

        LatestMessages.fromMessageMetaData(messageMetaDataList) mustBe expectedResult
      }

      "must order by dateTime and return the top 2 results with mixed date times" in {

        val messageMetaData1 = MessageMetaData(PositiveAcknowledgement, LocalDateTime.now())
        val messageMetaData2 = MessageMetaData(MrnAllocated, LocalDateTime.now().minusSeconds(1))
        val messageMetaData3 = MessageMetaData(DeclarationRejected, LocalDateTime.now().minusDays(2))
        val messageMetaData4 = MessageMetaData(ControlDecisionNotification, LocalDateTime.now().minusMonths(3))
        val messageMetaData5 = MessageMetaData(NoReleaseForTransit, LocalDateTime.now().minusYears(4))

        val messageMetaDataList = Seq(messageMetaData3, messageMetaData5, messageMetaData1, messageMetaData2, messageMetaData4)

        val expectedResult = LatestMessages(messageMetaData1, Some(messageMetaData2))

        LatestMessages.fromMessageMetaData(messageMetaDataList) mustBe expectedResult
      }
    }

    "fromMessages" - {

      "must order by dateTime and return the top 2 results by days" in {

        forAll(Gen.listOfN(5, arbitrary[MessageWithStatus])) {
          messages =>
            val message1 = messages.head.copy(dateTime = LocalDateTime.now().minusDays(3))
            val message2 = messages.tail(0).copy(dateTime = LocalDateTime.now().minusDays(4))
            val message3 = messages.tail(1).copy(dateTime = LocalDateTime.now().minusDays(5))
            val message4 = messages.tail(2).copy(dateTime = LocalDateTime.now().minusDays(1))
            val message5 = messages.tail(3).copy(dateTime = LocalDateTime.now().minusDays(2))

            val messagesToList = Seq(message1, message2, message3, message4, message5)

            val expectedMessage1 = MessageMetaData(message4.messageType, message4.dateTime)
            val expectedMessage2 = MessageMetaData(message5.messageType, message5.dateTime)

            val expectedResult = LatestMessages(expectedMessage1, Some(expectedMessage2))

            LatestMessages.fromMessages(messagesToList) mustBe expectedResult
        }
      }

      "must order by dateTime and return the top 2 results by seconds" in {

        forAll(Gen.listOfN(5, arbitrary[MessageWithStatus])) {
          messages =>
            val message1 = messages.head.copy(dateTime = LocalDateTime.now().minusSeconds(3))
            val message2 = messages.tail(0).copy(dateTime = LocalDateTime.now().minusSeconds(4))
            val message3 = messages.tail(1).copy(dateTime = LocalDateTime.now().minusSeconds(5))
            val message4 = messages.tail(2).copy(dateTime = LocalDateTime.now().minusSeconds(1))
            val message5 = messages.tail(3).copy(dateTime = LocalDateTime.now().minusSeconds(2))

            val messagesToList = Seq(message1, message2, message3, message4, message5)

            val expectedMessage1 = MessageMetaData(message4.messageType, message4.dateTime)
            val expectedMessage2 = MessageMetaData(message5.messageType, message5.dateTime)

            val expectedResult = LatestMessages(expectedMessage1, Some(expectedMessage2))

            LatestMessages.fromMessages(messagesToList) mustBe expectedResult
        }
      }

      "must order by dateTime and return the top 2 results by hours" in {

        forAll(Gen.listOfN(5, arbitrary[MessageWithStatus])) {
          messages =>
            val message1 = messages.head.copy(dateTime = LocalDateTime.now().minusHours(3))
            val message2 = messages.tail(0).copy(dateTime = LocalDateTime.now().minusHours(4))
            val message3 = messages.tail(1).copy(dateTime = LocalDateTime.now().minusHours(5))
            val message4 = messages.tail(2).copy(dateTime = LocalDateTime.now().minusHours(1))
            val message5 = messages.tail(3).copy(dateTime = LocalDateTime.now().minusHours(2))

            val messagesToList = Seq(message1, message2, message3, message4, message5)

            val expectedMessage1 = MessageMetaData(message4.messageType, message4.dateTime)
            val expectedMessage2 = MessageMetaData(message5.messageType, message5.dateTime)

            val expectedResult = LatestMessages(expectedMessage1, Some(expectedMessage2))

            LatestMessages.fromMessages(messagesToList) mustBe expectedResult
        }
      }

      "must order by dateTime and return the top 2 results by mixed date times" in {

        forAll(Gen.listOfN(5, arbitrary[MessageWithStatus])) {
          messages =>
            val message1 = messages.head.copy(dateTime = LocalDateTime.now().minusDays(3))
            val message2 = messages.tail(0).copy(dateTime = LocalDateTime.now().minusMonths(4))
            val message3 = messages.tail(1).copy(dateTime = LocalDateTime.now().minusMinutes(5))
            val message4 = messages.tail(2).copy(dateTime = LocalDateTime.now())
            val message5 = messages.tail(3).copy(dateTime = LocalDateTime.now().minusSeconds(2))

            val messagesToList = Seq(message1, message2, message3, message4, message5)

            val expectedMessage1 = MessageMetaData(message4.messageType, message4.dateTime)
            val expectedMessage2 = MessageMetaData(message5.messageType, message5.dateTime)

            val expectedResult = LatestMessages(expectedMessage1, Some(expectedMessage2))

            LatestMessages.fromMessages(messagesToList) mustBe expectedResult
        }
      }
    }
  }
}
